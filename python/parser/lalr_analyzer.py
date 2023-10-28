#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lalr_analyzer.py - 
#
# Created by skywind on 2015/01/13
# Last Modified: 2023/01/13 02:50:52
#
#======================================================================
import sys
import time
import collections
import enum
import pprint

import console
import internal
import grammar
import grammar_loader
import lrlib
import lalr
import lr1_analyzer


from enum import Enum

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF
from lalr import LALRItemSet, PSHARP
from lrlib import RulePtr, LRItemSet
from lr1_analyzer import LR1Analyzer


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = []


#----------------------------------------------------------------------
# internal
#----------------------------------------------------------------------
LOG_DEBUG = lambda *args: console.log(internal.LOG_LR_DEBUG, *args)
LOG_INFO = lambda *args: console.log(internal.LOG_LR, *args)
LOG_ERROR = lambda *args: console.log(internal.LOG_ERROR, *args)
LOG_VERBOSE = lambda *args: console.log(internal.LOG_VERBOSE, *args)


#----------------------------------------------------------------------
# LALR Analyzer
#----------------------------------------------------------------------
class LALRAnalyzer (object):

    def __init__ (self, g: Grammar):
        self.g: Grammar = g
        self.ga: GrammarAnalyzer = GrammarAnalyzer(self.g)
        self.la: LR1Analyzer = LR1Analyzer(self.g)
        self.state = {}         # state by uuid
        self.names = {}         # state by name
        self.link = {}          # state switch
        self.backlink = {}      #
        self.route = {}
        self.pending = collections.deque()
        self.dirty = set([])
        self.cache = {}

    def __len__ (self):
        return len(self.state)

    def __contains__ (self, key):
        if isinstance(key, LALRItemSet):
            return (key.name in self.names)
        elif isinstance(key, str):
            return (key in self.names)
        elif not hasattr(key, '__iter__'):
            raise TypeError('invalid type')
        name = LRItemSet.create_name(key)
        return (name in self.names)

    def __getitem__ (self, key):
        if isinstance(key, int):
            return self.state[key]
        elif isinstance(key, str):
            return self.names[key]
        elif isinstance(key, LRItemSet):
            return self.names[key.name]
        elif isinstance(key, LALRItemSet):
            return self.names[key.name]
        elif not hasattr(key, '__iter__'):
            raise TypeError('invalid type')
        name = LRItemSet.create_name(key)
        return self.names[name]

    def __iter__ (self):
        return self.state.__iter__()

    def append (self, state:LALRItemSet):
        if state in self:
            raise KeyError('conflict key')
        state.uuid = len(self.state)
        self.state[state.uuid] = state
        self.names[state.name] = state
        self.pending.append(state)
        return 0

    def clear (self):
        self.state.clear()
        self.names.clear()
        self.link.clear()
        self.backlink.clear()
        self.pending.clear()
        self.route.clear()
        self.cache.clear()
        return 0

    def process (self):
        self.clear()
        self.la.clear()
        self.ga.process()
        self.la.ga = self.ga
        error = self.ga.check_grammar()
        if error > 0:
            return 1
        if len(self.g) == 0:
            return 2
        if 'S^' not in self.g.symbol:
            self.g.argument()
        error = self.__LR0_build_states()
        if error > 0:
            return 3
        error = self.__build_propagate_route()
        if error > 0:
            return 4
        self.__build_lookahead()
        self.__build_LR1_state()
        self.tab = self.la.build_LR1_table()
        return 0

    def _LR0_closure (self, cc:LALRItemSet):
        cc.clear()
        for k in cc.kernel:
            cc.append(k)
        top = 0
        while 1:
            changes = 0
            limit = len(cc)
            while top < limit:
                A = cc.closure[top]
                top += 1
                B = A.next
                if B is None:
                    continue
                if B.term:
                    continue
                if B.name not in self.g.rule:
                    LOG_ERROR('no production rules for symbol %s'%B.name)
                for rule in self.g.rule[B.name]:
                    li = RulePtr(rule, 0, None)
                    if li not in cc:
                        cc.append(li)
                        changes += 1
            if not changes:
                break
        return cc

    def _LR0_goto (self, cc:LALRItemSet, X:Symbol):
        kernel = []
        for li in cc:
            if li.next is None:
                continue
            if li.next.name != X.name:
                continue
            np = li.advance()
            if np is None:
                continue
            kernel.append(np)
        if not kernel:
            return None
        nc = LALRItemSet(kernel)
        return self._LR0_closure(nc)

    def __LR0_try_goto (self, cc:LALRItemSet, X:Symbol):
        kernel = []
        for lp in cc:
            if lp.next is None:
                continue
            if lp.next.name != X.name:
                continue
            np = lp.advance()
            if np is None:
                continue
            kernel.append(np)
        if not kernel:
            return None
        return kernel

    def __LR0_build_states (self):
        self.clear()
        g = self.g
        assert g.start is not None
        assert g.start.name == 'S^'
        assert g.start.name in g.rule
        assert len(g.rule[g.start.name]) == 1
        rule = self.g.rule[g.start.name][0]
        lp = RulePtr(rule, 0)
        state = LALRItemSet([lp])
        self._LR0_closure(state)
        self.append(state)
        while 1:
            if len(self.pending) == 0:
                break
            state = self.pending.popleft()
            self.__LR0_update_state(state)
        return 0

    def __LR0_update_state (self, cc:LALRItemSet):
        changes = 0
        for symbol in cc.find_expecting_symbol():
            # print('expecting', symbol)
            kernel_list = self.__LR0_try_goto(cc, symbol)
            if not kernel_list:
                continue
            name = LRItemSet.create_name(kernel_list)
            if name in self:
                ns = self.names[name]
                self.__create_link(cc, ns, symbol)
                continue
            LOG_VERBOSE('create state %d'%len(self.state))
            ns = LALRItemSet(kernel_list)
            self._LR0_closure(ns)
            self.append(ns)
            self.__create_link(cc, ns, symbol)
            # print(ns.name)
            changes += 1
        return changes

    def __create_link (self, c1:LALRItemSet, c2:LALRItemSet, ss:Symbol):
        assert c1 is not None
        assert c2 is not None
        assert c1.uuid >= 0
        assert c2.uuid >= 0
        if c1.uuid not in self.link:
            self.link[c1.uuid] = {}
        if ss.name not in self.link[c1.uuid]:
            self.link[c1.uuid][ss.name] = c2.uuid
        else:
            if self.link[c1.uuid][ss.name] != c2.uuid:
                LOG_ERROR('conflict states')
        if c2.uuid not in self.backlink:
            self.backlink[c2.uuid] = {}
        if ss.name not in self.backlink[c2.uuid]:
            self.backlink[c2.uuid][ss.name] = c1.uuid
        return 0

    def _LR1_create_closure (self, kernel_list) -> LRItemSet:
        for n in kernel_list:
            if not isinstance(n, RulePtr):
                raise TypeError('kernel_list must be a list of RulePtr')
        cc = LRItemSet(kernel_list)
        self.la.closure(cc)
        return cc

    def _LALR_propagate_state (self, state:LALRItemSet) -> int:
        LOG_VERBOSE('propagate', state.uuid)
        for key, kernel in enumerate(state.kernel):
            rule_ptr = RulePtr(kernel.rule, kernel.index, PSHARP)
            closure = self._LR1_create_closure([rule_ptr])
            for _id, rp in enumerate(closure.closure):
                # print('RP', rp)
                expected: Symbol = rp.next
                if rp.satisfied:
                    continue
                elif expected is None:
                    LOG_ERROR('expecting lookahead symbol')
                    assert expected is not None
                assert state.uuid in self.link
                link = self.link[state.uuid]
                assert expected.name in link
                ns: LALRItemSet = self.state[link[expected.name]]
                # print('    ns: uuid', ns.uuid, 'kernel', [str(k) for k in ns.kernel])
                advanced: RulePtr = rp.advance()
                assert advanced
                found = -1
                advanced.lookahead = None
                # print('    advanced: ', advanced)
                for j, nk in enumerate(ns.kernel):
                    # print('nk', nk)
                    if advanced == nk:
                        found = j
                        break
                assert found >= 0
                if rp.lookahead is None:
                    LOG_ERROR('lookahead should not be None')
                    assert rp.lookahead is not None
                elif rp.lookahead == PSHARP:
                    if state.uuid not in self.route:
                        self.route[state.uuid] = {}
                    route = self.route[state.uuid]
                    if key not in route:
                        route[key] = []
                    route[key].append((ns.uuid, found))
                    # print('    new route: %s to %s'%(key, (ns.uuid, found)))
                else:
                    ns.lookahead[found].add(rp.lookahead)
        if state.uuid == 0:
            assert len(state.kernel) > 0
            kernel: RulePtr = state.kernel[0]
            assert kernel.rule.head.name == 'S^'
            state.lookahead[0].add(EOF)
        # print()
        return 0

    def __build_propagate_route (self):
        for uuid in self.state:
            state: LALRItemSet = self.state[uuid]
            state.shrink()
            state.dirty = True
            self._LALR_propagate_state(state)
            # break
        return 0

    def __propagate_state (self, state:LALRItemSet) -> int:
        changes = 0
        if state.uuid not in self.route:
            state.dirty = False
            if state.uuid in self.dirty:
                self.dirty.remove(state.uuid)
            return 0
        route = self.route[state.uuid]
        for key, kernel in enumerate(state.kernel):
            if key not in route:
                continue
            lookahead = state.lookahead[key]
            for new_uuid, kid in route[key]:
                cc: LALRItemSet = self.state[new_uuid]
                assert kid < len(cc.kernel)
                for symbol in lookahead:
                    if symbol not in cc.lookahead[kid]:
                        cc.lookahead[kid].add(symbol)
                        cc.dirty = True
                        self.dirty.add(cc.uuid)
                        changes += 1
        state.dirty = False
        if state.uuid in self.dirty:
            self.dirty.remove(state.uuid)
        return changes

    def __build_lookahead (self):
        self.dirty.clear()
        for uuid in self.state:
            self.dirty.add(uuid)
        while 1:
            if 0:
                changes = 0
                for state in self.state.values():
                    changes += self.__propagate_state(state)
                if changes == 0:
                    break
            else:
                changes = 0
                for uuid in list(self.dirty):
                    state = self.state[uuid]
                    changes += self.__propagate_state(state)
                if changes == 0:
                    break
        return 0

    def __build_LR1_state (self):
        for state in self.state.values():
            kernel_list = []
            LOG_VERBOSE('building LR1 state', state.uuid)
            for key, kernel in enumerate(state.kernel):
                lookahead = state.lookahead[key]
                for symbol in lookahead:
                    rp = RulePtr(kernel.rule, kernel.index, symbol)
                    kernel_list.append(rp)
            cc = LRItemSet(kernel_list)
            self.la.closure(cc)
            self.la.append(cc)
        self.la.link = self.link
        self.la.backlink = self.backlink
        return 0



#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/d4_49.txt')
        la = LALRAnalyzer(g)
        la.process()
        li = RulePtr(g[0], 0)
        cc = LALRItemSet([li])
        la.append(cc)
        cc = la._LR0_closure(cc)
        cc.print()
        c2 = la._LR0_goto(cc, Symbol("'*'", True))
        c2.print()
        return 0
    def test2():
        # g = grammar_loader.load_from_file('grammar/expr_bnf.txt')
        g = grammar_loader.load_from_file('grammar/d4_49.txt')
        la = LALRAnalyzer(g)
        la.process()
        for cc in la.state.values():
            cc.print()
        pprint.pprint(la.link)
        print()
        pprint.pprint(la.route)
        la.tab.print()
        return 0
    def test3():
        console.enable(internal.LOG_VERBOSE)
        # g = grammar_loader.load_from_file('grammar/expr_bnf.txt')
        g = grammar_loader.load_from_file('grammar/ansi_c.txt')
        la = LALRAnalyzer(g)
        t1 = time.time()
        la.process()
        print('state count', len(la.state))
        print('time usage', time.time() - t1, 'seconds')
        return 0
    test2()




