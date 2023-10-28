#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lr1_analyzer.py - 
#
# Created by skywind on 2015/01/10
# Last Modified: 2023/01/10 02:18:27
#
#======================================================================
import sys
import collections
import console
import internal
import grammar
import grammar_loader

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF
from lrlib import RulePtr, LRItemSet, ActionName, Action, LRTable


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = []


#----------------------------------------------------------------------
# internal
#----------------------------------------------------------------------
LOG_DEBUG = lambda *args: console.log(internal.LOG_LR_DEBUG, *args)
LOG_ERROR = lambda *args: console.log(internal.LOG_ERROR, *args)
LOG_INFO = lambda *args: console.log(internal.LOG_LR, *args)
LOG_VERBOSE = lambda *args: console.log(internal.LOG_VERBOSE, *args)

# all log channels are enabled by default
# turn on/off this to enable/disable debug message
if 1:
    console.disable(internal.LOG_LR_DEBUG)
    console.disable(internal.LOG_VERBOSE)

# print(f'LOG LEVEL: {console.LOG_LEVEL}')


#----------------------------------------------------------------------
# LR(1) Analyzer
#----------------------------------------------------------------------
class LR1Analyzer (object):

    def __init__ (self, g: Grammar):
        self.g = g
        self.ga = GrammarAnalyzer(self.g)
        self.verbose = 2
        self.state = {}         # state by uuid
        self.names = {}         # state by name
        self.link = {}          # state switch
        self.backlink = {}      #
        self.tab = None         # LR table
        self.pending = collections.deque()

    def process (self):
        self.clear()
        self.ga.process()
        error = self.ga.check_grammar()
        if error > 0:
            return 1
        if len(self.g) == 0:
            return 2
        if 'S^' not in self.g.symbol:
            self.g.argument()
        hr = self.__build_states()
        if hr != 0:
            return 3
        hr = self.__build_table()
        if hr != 0:
            return 4
        return 0

    def __len__ (self):
        return len(self.state)

    def __contains__ (self, key):
        if isinstance(key, LRItemSet):
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
        elif not hasattr(key, '__iter__'):
            raise TypeError('invalid type')
        name = LRItemSet.create_name(key)
        return self.names[name]

    def __iter__ (self):
        return self.state.__iter__()

    def append (self, state:LRItemSet):
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
        return 0

    def closure (self, cc:LRItemSet) -> LRItemSet:
        cc.clear()
        for n in cc.kernel:
            if n.name not in cc:
                cc.append(n)
        if 1:
            LOG_DEBUG('')
            LOG_DEBUG('-' * 72)
            LOG_DEBUG('CLOSURE init')
        top = 0
        while 1:
            changes = 0
            limit = len(cc.closure)
            while top < limit:
                A: RulePtr = cc.closure[top]
                top += 1
                B: Symbol = A.next
                if B is None:
                    continue
                if B.term:
                    continue
                if B.name not in self.g.rule:
                    LOG_ERROR('no production rules for symbol %s'%B.name)
                    raise GrammarError('no production rules for symbol %s'%B.name)
                if 1:
                    LOG_DEBUG('CLOSURE iteration') 
                    LOG_DEBUG(f'A={A} B={B}')
                after = A.after_list(1)
                if A.lookahead is not None:
                    after.append(A.lookahead)
                first = self.ga.vector_first_set(after)
                first = [grammar.load_symbol(n) for n in first]
                for f in first:
                    if f.name != '':
                        f.term = True
                if 1:
                    LOG_DEBUG('after:', after)
                    LOG_DEBUG('first:', first)
                for rule in self.g.rule[B.name]:
                    for term in first:
                        rp = RulePtr(rule, 0, term)
                        if rp.name not in cc:
                            cc.append(rp)
                            changes += 1
            if changes == 0:
                break
        return cc

    def goto (self, cc:LRItemSet, X:Symbol):
        kernel = []
        for rp in cc:
            if rp.next is None:
                continue
            if rp.next.name != X.name:
                continue
            np = rp.advance()
            if np is None:
                continue
            kernel.append(np)
        if not kernel:
            return None
        nc = LRItemSet(kernel)
        return self.closure(nc)

    def __try_goto (self, cc:LRItemSet, X:Symbol):
        kernel = []
        for rp in cc:
            if rp.next is None:
                continue
            if rp.next.name != X.name:
                continue
            np = rp.advance()
            if np is None:
                continue
            kernel.append(np)
        if not kernel:
            return None
        return kernel

    def __build_states (self):
        self.clear()
        g = self.g
        assert g.start is not None
        assert g.start.name == 'S^'
        assert g.start.name in g.rule
        assert len(g.rule[g.start.name]) == 1
        rule = self.g.rule[g.start.name][0]
        rp = RulePtr(rule, 0, EOF)
        state = LRItemSet([rp])
        self.closure(state)
        self.append(state)
        while 1:
            if 0:
                changes = 0
                for state in list(self.state.values()):
                    changes += self.__update_state(state)
                if not changes:
                    break
            else:
                if len(self.pending) == 0:
                    break
                state = self.pending.popleft()
                self.__update_state(state)
        return 0

    def __update_state (self, cc:LRItemSet):
        changes = 0
        for symbol in cc.find_expecting_symbol():
            # print('expecting', symbol)
            kernel_list = self.__try_goto(cc, symbol)
            if not kernel_list:
                continue
            name = LRItemSet.create_name(kernel_list)
            if name in self:
                ns = self.names[name]
                self.__create_link(cc, ns, symbol)
                continue
            LOG_VERBOSE('create state %d'%len(self.state))
            ns = LRItemSet(kernel_list)
            self.closure(ns)
            self.append(ns)
            self.__create_link(cc, ns, symbol)
            # print(ns.name)
            changes += 1
        return changes

    def __create_link (self, c1:LRItemSet, c2:LRItemSet, ss:Symbol):
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

    def __build_table (self):
        heading = [n for n in self.g.symbol.values()]
        self.tab = LRTable(heading)
        tab: LRTable = self.tab
        # tab.mode = 1
        if 0:
            import pprint
            pprint.pprint(self.link)
        for state in self.state.values():
            uuid = state.uuid
            link = self.link.get(uuid, None)
            LOG_VERBOSE(f'build table for I{state.uuid}')
            # LOG_VERBOSE(
            for rp in state.closure:
                rp: RulePtr = rp
                if rp.satisfied:
                    # LOG_VERBOSE("  satisfied:", rp)
                    if rp.rule.head.name == 'S^':
                        if len(rp.rule.body) == 1:
                            action = Action(ActionName.ACCEPT, 0)
                            action.rule = rp.rule
                            tab.add(uuid, rp.lookahead.name, action)
                        else:
                            LOG_ERROR('error accept:', rp)
                    else:
                        action = Action(ActionName.REDUCE, rp.rule.index)
                        action.rule = rp.rule
                        tab.add(uuid, rp.lookahead.name, action)
                elif rp.next.name in link:
                    target = link[rp.next.name]
                    action = Action(ActionName.SHIFT, target, rp.rule)
                    tab.add(uuid, rp.next.name, action)
                else:
                    LOG_ERROR('error link')
        return 0

    def build_LR1_table (self) -> LRTable:
        self.__build_table()
        return self.tab


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/expr_bnf.txt')
        la = LR1Analyzer(g)
        la.process()
        g.print()
        return 0
    def test2():
        g = grammar_loader.load_from_file('grammar/d4_40.txt')
        la = LR1Analyzer(g)
        la.process()
        g.print()
        rp = RulePtr(g.production[0], 0, EOF)
        print(g.terminal)
        print(list(g.rule.keys()))
        cc = LRItemSet([rp])
        cc = la.closure(cc)
        cc.print()
        nc = la.goto(cc, Symbol("'c'", True))
        nc.print()
        return 0
    def test3():
        # g = grammar_loader.load_from_file('grammar/d4_40.txt')
        # g = grammar_loader.load_from_file('grammar/expr_bnf.txt')
        g = grammar_loader.load_from_file('grammar/example_1.txt')
        la = LR1Analyzer(g)
        la.process()
        import pprint
        pprint.pprint(la.link)
        # g.print()
        print(len(la.state))
        tab: LRTable = la.tab
        tab.print()
        return 0
    def test4():
        g = grammar_loader.load_from_file('grammar/ansi_c.txt')
        # g = grammar_loader.load_from_file('grammar/example_1.txt')
        la = LR1Analyzer(g)
        import time
        t1 = time.time()
        la.process()
        print('state count', len(la.state))
        print('time usage', time.time() - t1, 'seconds')

    test3()





