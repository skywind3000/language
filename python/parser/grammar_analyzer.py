#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# grammar_analysis.py - 
#
# Created by skywind on 2015/01/08
# Last Modified: 2019/01/08 18:09:01
#
#======================================================================
import sys
import copy
import cstring
import grammar
import grammar_loader
import internal
import console

from grammar import Symbol, Vector, Production, Grammar, GrammarError


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['GrammarAnalyzer', 'SymbolInfo', 'EPSILON', 'EOF']


#----------------------------------------------------------------------
# marks
#----------------------------------------------------------------------
MARK_UNVISITED = 0
MARK_VISITING = 1
MARK_VISITED = 2

EPSILON = Symbol('', False)
EOF = Symbol('$', True)


#----------------------------------------------------------------------
# 符号信息
#----------------------------------------------------------------------
class SymbolInfo (object):

    def __init__ (self, symbol):
        self.symbol = symbol
        self.mark = MARK_UNVISITED
        self.rules = []
        self.rule_number = 0
        self.is_epsilon = None
        self.has_epsilon = None

    def __copy__ (self):
        obj = SymbolInfo(self.symbol)
        return obj

    def __deepcopy__ (self):
        return self.__copy__()

    @property
    def is_terminal (self):
        return self.symbol.term

    @property
    def name (self):
        return self.symbol.name

    def reset (self):
        self.mark = MARK_UNVISITED

    def check_epsilon (self):
        if len(self.rules) == 0:
            return 1
        count = 0
        for rule in self.rules:
            if rule.is_epsilon:
                count += 1
        if count == len(rule):
            return 2
        if count > 0:
            return 1
        return 0


#----------------------------------------------------------------------
# analyzer
#----------------------------------------------------------------------
class GrammarAnalyzer (object):

    def __init__ (self, g: Grammar):
        assert g is not None
        self.g = g
        self.info = {}
        self.epsilon = Symbol('')
        self.FIRST = {}
        self.FOLLOW = {}
        self.terminal = {}
        self.nonterminal = {}
        self.verbose = 2

    def process (self, expand_action = True):
        if expand_action:
            self.__argument_semantic_action()
        if self.g._dirty:
            self.g.update()
        self.__build_info()
        self.__update_epsilon()
        self.__update_first_set()
        self.__update_follow_set()
        self.__check_integrity()
        return 0

    def __build_info (self):
        self.info.clear()
        self.terminal.clear()
        self.nonterminal.clear()
        g = self.g
        for name in g.symbol:
            info = SymbolInfo(g.symbol[name])
            self.info[name] = info
            info.reset()
            rules = g.rule.get(name, [])
            info.rule_number = len(rules)
            info.rules = rules
            if info.is_terminal:
                self.terminal[info.name] = info.symbol
            else:
                self.nonterminal[info.name] = info.symbol
        return 0

    def __update_first_set (self):
        self.FIRST.clear()
        for name in self.g.symbol:
            symbol = self.g.symbol[name]
            if symbol.term:
                self.FIRST[name] = set([name])
            else:
                self.FIRST[name] = set()
        self.FIRST['$'] = set(['$'])
        self.FIRST['#'] = set(['#'])
        while 1:
            changes = 0
            for symbol in self.nonterminal:
                info = self.info[symbol]
                for rule in info.rules:
                    first = self.__calculate_first_set(rule.body)
                    for n in first:
                        if n not in self.FIRST[symbol]:
                            self.FIRST[symbol].add(n)
                            changes += 1
            if not changes:
                break
        return 0

    def __calculate_first_set (self, vector):
        output = set([])
        index = 0
        for symbol in vector:
            if symbol.term:
                output.add(symbol.name)
                break
            if symbol.name not in self.FIRST:
                for key in self.FIRST.keys():
                    print('FIRST:', key)
                raise ValueError('FIRST set does not contain %r'%symbol.name)
            for name in self.FIRST[symbol.name]:
                if name != EPSILON:
                    output.add(name)
            if EPSILON not in self.FIRST[symbol.name]:
                break
            index += 1
        if index >= len(vector):
            output.add(EPSILON.name)
        return output

    def __update_follow_set (self):
        self.FOLLOW.clear()
        start = self.g.start
        if not self.g.start:
            if len(self.g) > 0:
                start = self.g[0].head
        if not start:
            internal.echo_error('start point is required')
            return 0
        FOLLOW = self.FOLLOW
        for n in self.nonterminal:
            FOLLOW[n] = set([])
        FOLLOW[start.name] = set(['$'])
        while 1:
            changes = 0
            for p in self.g:
                for i, symbol in enumerate(p.body):
                    if symbol.term:
                        continue
                    follow = p.body[i + 1:]
                    first = self.vector_first_set(follow)
                    epsilon = False
                    for n in first:
                        if n != EPSILON.name:
                            if n not in FOLLOW[symbol.name]:
                                FOLLOW[symbol.name].add(n)
                                changes += 1
                        else:
                            epsilon = True
                    if epsilon or i == len(p.body) - 1:
                        for n in FOLLOW[p.head]:
                            if n not in FOLLOW[symbol.name]:
                                FOLLOW[symbol.name].add(n)
                                changes += 1
            if not changes:
                break
        return 0

    def __update_epsilon (self):
        g = self.g
        for info in self.info.values():
            if info.symbol.term:
                info.is_epsilon = False
                info.has_epsilon = False
                info.mark = MARK_VISITED
            elif len(info.rules) == 0:
                info.is_epsilon = False
                info.has_epsilon = False
                info.mark = MARK_VISITED
            else:
                is_count = 0
                size = len(info.rules)
                for p in info.rules:
                    if p.is_epsilon:
                        is_count += 1
                if is_count >= size:
                    info.is_epsilon = True
                    info.has_epsilon = True
                    info.mark = MARK_VISITED
                elif is_count > 0:
                    info.has_epsilon = True

        while True:
            count = 0
            for p in g.production:
                count += self.__update_epsilon_production(p)
            for info in self.info.values():
                count += self.__update_epsilon_symbol(info)
            if not count:
                break
        return 0

    def __update_epsilon_symbol (self, info: SymbolInfo):
        count = 0
        if info.symbol.term:
            return 0
        elif info.is_epsilon is not None:
            if info.has_epsilon is not None:
                return 0
        is_count = 0
        isnot_count = 0
        has_count = 0
        hasnot_count = 0
        for p in info.rules:
            if p.is_epsilon:
                is_count += 1
            elif p.is_epsilon is not None:
                isnot_count += 1
            if p.has_epsilon:
                has_count += 1
            elif p.has_epsilon is not None:
                hasnot_count += 1
        size = len(info.rules)
        if info.is_epsilon is None:
            if is_count >= size:
                info.is_epsilon = True
                info.has_epsilon = True
                count += 1
            elif isnot_count >= size:
                info.is_epsilon = False
                info.has_epsilon = False
                count += 1
        if info.has_epsilon is None:
            if has_count > 0:
                info.has_epsilon = True
                count += 1
            elif hasnot_count >= size:
                info.has_epsilon = False
                count += 1
        return count

    def __update_epsilon_production (self, p: Production):
        count = 0
        if (p.is_epsilon is not None) and (p.has_epsilon is not None):
            return 0
        if p.leftmost_terminal() is not None:
            if p.is_epsilon is None:
                p.is_epsilon = False
                count += 1
            if p.has_epsilon is None:
                p.has_epsilon = False
                count += 1
            return count
        is_count = 0
        isnot_count = 0
        has_count = 0
        hasnot_count = 0
        for n in p.body:
            info = self.info[n.name]
            if info.is_epsilon:
                is_count += 1
            elif info.is_epsilon is not None:
                isnot_count += 1
            if info.has_epsilon:
                has_count += 1
            elif info.has_epsilon is not None:
                hasnot_count += 1
        if p.is_epsilon is None:
            if is_count >= len(p.body):
                p.is_epsilon = True
                p.has_epsilon = True
                count += 1
            elif isnot_count > 0:
                p.is_epsilon = False
                count += 1
        if p.has_epsilon is None:
            if has_count >= len(p.body):
                p.has_epsilon = True
                count += 1
            elif hasnot_count > 0:
                p.has_epsilon = False
                count += 1
        return count

    def vector_is_epsilon (self, vector: Vector):
        if vector.leftmost_terminal() is not None:
            return False
        is_count = 0
        isnot_count = 0
        for symbol in vector:
            if symbol.name not in self.info:
                continue
            info = self.info[symbol.name]
            if info.is_epsilon:
                is_count += 1
            elif info.is_epsilon is not None:
                isnot_count += 1
        if is_count >= len(vector):
            return True
        return False

    def vector_has_epsilon (self, vector: Vector):
        if vector.leftmost_terminal() is not None:
            return False
        is_count = 0
        isnot_count = 0
        has_count = 0
        hasnot_count = 0
        for symbol in vector:
            if symbol.name not in self.info:
                continue
            info = self.info[symbol.name]
            if info.is_epsilon:
                is_count += 1
            elif info.is_epsilon is not None:
                isnot_count += 1
            if info.has_epsilon:
                has_count += 1
            elif info.has_epsilon is not None:
                hasnot_count += 1
        size = len(vector)
        if (is_count >= size) or (has_count >= size):
            return True
        return False

    def vector_first_set (self, vector):
        return self.__calculate_first_set(vector)

    def __integrity_error (self, *args):
        text = ' '.join(args)
        internal.echo_error('integrity: ' + text)
        return 0

    def symbol_error (self, symbol, *args):
        if self.verbose < 1:
            return 0
        if symbol is None:
            return internal.echo_error(' '.join(args), self.g.file_name, 1)
        return internal.symbol_error(self.g, symbol, *args)

    def symbol_warning (self, symbol, *args):
        if self.verbose < 2:
            return 0
        if symbol is None:
            return internal.echo_warning(' '.join(args), self.g.file_name, 1)
        return internal.symbol_warning(self.g, symbol, *args)

    def __check_integrity (self):
        error = 0
        for info in self.info.values():
            symbol = info.symbol
            if symbol.term:
                continue
            name = symbol.name
            first = self.FIRST[name]
            if EPSILON.name in first:
                if len(first) == 1:
                    if not info.is_epsilon:
                        t = 'symbol %s is not epsilon but '%name
                        t += 'first set only contains epsilon'
                        self.__integrity_error(t)
                        error += 1
                    if not info.has_epsilon:
                        t = 'symbol %s has not epsilon but '%name
                        t += 'first set only contains epsilon'
                        self.__integrity_error(t)
                        error += 1
                elif len(first) > 0:
                    if info.is_epsilon:
                        t = 'symbol %s is epsilon but '%name
                        t += 'first set contains more than epsilon'
                        self.__integrity_error(t)
                        error += 1
                    if not info.has_epsilon:
                        t = 'symbol %s has not epsilon but '%name
                        t += 'first set contains epsilon'
                        self.__integrity_error(t)
                        error += 1
            else:
                if info.is_epsilon:
                    t = 'symbol %s is epsilon but '%name
                    t += 'first set does not contains epsilon'
                    self.__integrity_error(t)
                    error += 1
                if info.has_epsilon:
                    t = 'symbol %s has epsilon but '%name
                    t += 'first set does not contains epsilon'
                    self.__integrity_error(t)
                    error += 1
        if error and 0:
            sys.exit(1)
        return error

    def clear_mark (self, init = MARK_UNVISITED):
        for info in self.info.values():
            info.mark = init
        return 0

    def __iter_child (self, symbol):
        if symbol in self.g.rule:
            for rule in self.g.rule[symbol]:
                for n in rule.body:
                    yield n.name
        return None

    def find_reachable (self, parents):
        output = []
        if parents is None:
            if self.g.start is None:
                return set()
            roots = self.g.start.name
        elif isinstance(parents, str):
            roots = [parents]
        elif isinstance(parents, Symbol):
            roots = [parents.name]
        else:
            roots = parents
        for symbol in internal.bfs(roots, self.__iter_child):
            output.append(symbol)
        return output

    def find_undefined_symbol (self):
        undefined = set([])
        for sname in self.g.symbol:
            if sname in self.g.terminal:
                continue
            if sname not in self.g.rule:
                if sname not in undefined:
                    undefined.add(sname)
            elif len(self.g.rule[sname]) == 0:
                if sname not in undefined:
                    undefined.add(sname)
        return list(undefined)

    def find_terminated_symbol (self):
        terminated = set([])
        for symbol in self.g.symbol.values():
            if symbol.term:
                terminated.add(symbol.name)
        while 1:
            changes = 0
            for symbol in self.g.symbol.values():
                if symbol.name in terminated:
                    continue
                elif symbol.name not in self.g.rule:
                    continue
                for rule in self.g.rule[symbol.name]:
                    can_terminate = True
                    for n in rule.body:
                        if n.name not in terminated:
                            can_terminate = False
                            break
                    if can_terminate:
                        if symbol.name not in terminated:
                            terminated.add(symbol.name)
                            changes += 1
                        break
            if not changes:
                break
        return list(terminated)

    def check_grammar (self):
        self.error = 0
        self.warning = 0
        if len(self.g) == 0:
            self.symbol_error(None, 'no rules has been defined')
            self.error += 1
        for n in self.find_undefined_symbol():
            self.symbol_error(n, 'symbol %r is used, but is not defined as a token and has no rules'%n)
            self.error += 1
        smap = set(self.find_reachable(self.g.start))
        for symbol in self.g.symbol.values():
            if symbol.term:
                continue
            if symbol.name not in smap:
                self.symbol_warning(symbol, 'nonterminal \'%s\' useless in grammar'%symbol)
                self.warning += 1
        if self.g.start:
            if self.g.start.term:
                t = 'start symbol %s is a token'
                self.symbol_error(self.g.start, t)
                self.error += 1
            terminated = self.find_terminated_symbol()
            # print('terminated', terminated)
            if self.g.start.name not in terminated:
                t = 'start symbol %s does not derive any sentence'%self.g.start.name
                self.symbol_error(self.g.start, t)
                self.error += 1
        else:
            self.symbol_error(None, 'start symbol is not defined')
            self.error += 1
        return self.error

    # 将 L 型 SDT （即生成式里有语法动作的）转换为 S 型纯后缀的模式
    def __argument_semantic_action (self):
        rules = []
        anchors = []
        name_id = 1
        for rule in self.g.production:
            rule:Production = rule
            anchor = self.g.anchor_get(rule)
            if not rule.action:
                rules.append(rule)
                anchors.append(anchor)
                continue
            count = 0
            for key in rule.action.keys():
                if key < len(rule):
                    count += 1
            if count == 0:
                rules.append(rule)
                anchors.append(anchor)
                continue
            body = []
            children = []
            for pos, symbol in enumerate(rule.body):
                if pos in rule.action:
                    head = Symbol('M@%d'%name_id, False)
                    name_id += 1
                    child = Production(head, [])
                    child.action = {}
                    child.action[0] = []
                    stack_pos = len(body)
                    for act in rule.action[pos]:
                        child.action[0].append((act[0], stack_pos))
                    child.parent = len(rules)
                    children.append(child)
                    body.append(head)
                    self.g.anchor_set(head, anchor[0], anchor[1])
                body.append(symbol)
            root = Production(rule.head, body)
            root.precedence = rule.precedence
            action = {}
            stack_pos = len(body)
            keys = list(filter(lambda k: k >= len(rule), rule.action.keys()))
            keys.sort()
            # print('keys', keys)
            for key in keys:
                assert key >= len(rule)
                for act in rule.action[key]:
                    if stack_pos not in action:
                        action[stack_pos] = []
                    action[stack_pos].append((act[0], stack_pos))
            if action:
                root.action = action
            rules.append(root)
            anchors.append(anchor)
            for child in children:
                rules.append(child)
                anchors.append(anchor)
                child.parent = root
        self.g.production.clear()
        for pos, rule in enumerate(rules):
            self.g.append(rule)
            anchor = anchors[pos]
            self.g.anchor_set(rule, anchor[0], anchor[1])
        self.g.update()
        return 0

    def set_to_text (self, s):
        t = []
        if len(s) == 0:
            return '{ }'
        for n in s:
            t.append((n == '') and '<empty>' or n)
        return '{ %s }'%(', '.join(t),)

    def print_epsilon (self):
        rows = []
        rows.append(['Symbol', 'Is Epsilon', 'Has Epsilon'])
        for info in self.info.values():
            eis = info.is_epsilon and 1 or 0
            ehas = info.has_epsilon and 1 or 0
            rows.append([info.name, eis, ehas])
        text = cstring.tabulify(rows, 1)
        print(text)
        print()
        return 0

    def print_first (self):
        rows = []
        rows.append(['Symbol X', 'First[X]', 'Follow[X]'])
        for name in self.nonterminal:
            t1 = self.set_to_text(self.FIRST[name])
            t2 = self.set_to_text(self.FOLLOW[name])
            rows.append([name, t1, t2])
        text = cstring.tabulify(rows, 1)
        print(text)
        print()
        return 0


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/test_bnf.txt')
        # g = grammar_loader.load_from_file('grammar/test_epsilon.txt')
        ga = GrammarAnalyzer(g)
        g.argument()
        # g.print()
        ga.process()
        ga.print_epsilon()
        ga.print_first()
        print(ga.find_reachable(g.start))
        return 0
    def test2():
        s = set()
        s.add('term')
        s.add(Symbol('ff'))
        sym = Symbol('term', True)
        print(sym in s)
        print('ff' in s)
        print(s)
        return 0
    def test3():
        g = grammar_loader.load_from_file('grammar/test_first.txt')
        ga = GrammarAnalyzer(g)
        g.print()
        ga.process()
        ga.print_first()
        print(ga.find_reachable([g.start.name]))
    def test4():
        g = grammar_loader.load_from_file('grammar/test_bnf.txt')
        # print(g.code_line)
        # g = grammar_loader.load_from_file('grammar/test_epsilon.txt')
        ga = GrammarAnalyzer(g)
        ga.check_grammar()
    def test5():
        BNF = '''
        E: E '+' T | T;
        T: T '*' F | F ;
        F: number | '(' E ')' ;
        %token number
        # F: X;
        '''
        g = grammar_loader.load_from_string(BNF)
        ga = GrammarAnalyzer(g)
        ga.check_grammar()
    def test6():
        g = grammar_loader.load_from_file('grammar/ansi_c.txt')
        ga = GrammarAnalyzer(g)
        ga.check_grammar()
        g.argument()
        # g.print()
        ga.process()
        return 0
    def test7():
        g = grammar_loader.load_from_file('grammar/test_action.txt')
        g.print(action = True)
        print()
        ga = GrammarAnalyzer(g)
        ga.process()
        g.print(action = True)
        print()
        # import pprint
        # pprint.pprint(g._anchor)
        print(g.anchor_get('M@2'))
        print(g.anchor_get('M@3'))
        print(g.anchor_get('number'))
        return 0
    test7()



