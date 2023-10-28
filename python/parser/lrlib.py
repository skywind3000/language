#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lrlib.py - 
#
# Created by skywind on 2015/01/10
# Last Modified: 2023/01/10 00:21:53
#
#======================================================================
import sys
import base64
import copy

import console
import cstring
import internal
import grammar

from enum import Enum, IntEnum

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['RulePtr', 'LRItemSet', 'ActionName', 'Action', 'LRTable' ]


#----------------------------------------------------------------------
# internal
#----------------------------------------------------------------------
LOG_DEBUG = lambda *args: console.log(internal.LOG_LR_DEBUG, *args)
LOG_INFO = lambda *args: console.log(internal.LOG_LR, *args)
LOG_ERROR = lambda *args: console.log(internal.LOG_ERROR, *args)
LOG_VERBOSE = lambda *args: console.log(internal.LOG_VERBOSE, *args)


#----------------------------------------------------------------------
# RulePtr: Universal LR Item for LR(0), SLR, LR(1), and LALR
#----------------------------------------------------------------------
class RulePtr (object):

    def __init__ (self, production: Production, index: int, lookahead = None):
        assert index <= len(production)
        self.rule = production
        self.index = index
        self.lookahead = lookahead    # None or a string
        self.__name = None
        self.__hash = None
        self.__text = None

    def __len__ (self) -> int:
        return len(self.rule)

    def __contains__ (self, symbol) -> bool:
        return (symbol in self.rule)

    def __getitem__ (self, key) -> Symbol:
        return self.rule[key]

    @property
    def next (self) -> Symbol:
        if self.index >= len(self.rule.body):
            return None
        return self.rule.body[self.index]

    def advance (self):
        if self.index >= len(self.rule.body):
            return None
        return RulePtr(self.rule, self.index + 1, self.lookahead)

    @property
    def satisfied (self) -> bool:
        return (self.index >= len(self.rule))

    def __str__ (self) -> str:
        if self.__text is not None:
            return self.__text
        before = [ x.name for x in self.rule.body[:self.index] ]
        after = [ x.name for x in self.rule.body[self.index:] ]
        t = '%s * %s'%(' '.join(before), ' '.join(after))
        if self.lookahead is not None:
            t += ', ' + str(self.lookahead)
        self.__text = '<%s : %s>'%(self.rule.head, t)
        return self.__text

    def __repr__ (self) -> str:
        return '%s(%r, %r)'%(type(self).__name__, self.rule, self.index)

    def __copy__ (self):
        obj = RulePtr(self.rule, self.index, self.lookahead)
        obj.__name = self.name
        obj.__hash = self.hash
        return obj

    def __deepcopy__ (self):
        return self.__copy__()

    def __hash__ (self) -> int:
        if self.__hash is None:
            self.__hash = hash((hash(self.rule), self.index, self.lookahead))
        return self.__hash

    def __eq__ (self, rp) -> bool:
        assert isinstance(rp, RulePtr)
        if (self.index == rp.index) and (self.lookahead == rp.lookahead):
            return (self.rule == rp.rule)
        return False

    def __ne__ (self, rp) -> bool:
        return (not self == rp)

    def __ge__ (self, rp) -> bool:
        if self.index > rp.index:
            return True
        elif self.index < rp.index:
            return False
        if (self.lookahead is not None) or (rp.lookahead is not None):
            s1 = repr(self.lookahead)
            s2 = repr(rp.lookahead)
            if s1 > s2:
                return True
            if s1 < s2:
                return False
        return self.rule >= rp.rule

    def __gt__ (self, rp) -> bool:
        if self.index > rp.index:
            return True
        elif self.index < rp.index:
            return False
        if (self.lookahead is not None) or (rp.lookahead is not None):
            s1 = repr(self.lookahead)
            s2 = repr(rp.lookahead)
            if s1 > s2:
                return True
            if s1 < s2:
                return False
        return self.rule > rp.rule

    def __le__ (self, rp) -> bool:
        return (not (self > rp))

    def __lt__ (self, rp) -> bool:
        return (not (self >= rp))

    @property
    def name (self) -> str:
        if self.__name is None:
            self.__name = self.__str__()
        return self.__name

    def after_list (self, inc = 0) -> list:
        return [n for n in self.rule.body[self.index + inc:]]


#----------------------------------------------------------------------
# Universal ItemSet for LR(0), SLR, LR(1)
#----------------------------------------------------------------------
class LRItemSet (object):

    def __init__ (self, kernel_source):
        self.kernel = LRItemSet.create_kernel(kernel_source)
        self.closure = []
        self.checked = {}
        self.__hash = None
        self.__name = None
        self.uuid = -1

    @staticmethod
    def create_kernel (kernel_source):
        klist = [n for n in kernel_source]
        klist.sort()
        return tuple(klist)

    @staticmethod
    def create_name (kernel_source):
        knl = LRItemSet.create_kernel(kernel_source)
        text = ', '.join([str(n) for n in knl])
        h = hash(knl) % 919393
        return 'C' + str(h) + '(' + text + ')'

    def __len__ (self):
        return len(self.closure)

    def __getitem__ (self, key):
        if isinstance(key, str):
            index = self.checked[key]
            return self.closure[index]
        return self.closure[key]

    def __contains__ (self, key):
        if isinstance(key, int):
            return ((key >= 0) and (key < len(self.closure)))
        elif isinstance(key, RulePtr):
            return (key.name in self.checked)
        elif isinstance(key, str):
            return (key in self.checked)
        return False

    def clear (self):
        self.closure.clear()
        self.checked.clear()
        return 0

    def __hash__ (self):
        if self.__hash is None:
            self.__hash = hash(self.kernel)
        return self.__hash

    def __eq__ (self, obj):
        assert isinstance(obj, LRItemSet)
        if hash(self) != hash(obj):
            return False
        return (self.kernel == obj.kernel)

    def __ne__ (self, obj):
        return (not (self == obj))

    @property
    def name (self):
        if self.__name is None:
            self.__name = LRItemSet.create_name(self.kernel)
        return self.__name

    def append (self, item: RulePtr):
        if item.name in self.checked:
            LOG_ERROR('duplicated item:', item)
            assert item.name not in self.checked
            return -1
        self.checked[item.name] = len(self.closure)
        self.closure.append(item)
        return 0

    def __iter__ (self):
        return self.closure.__iter__()

    def find_expecting_symbol (self):
        output = []
        checked = set([])
        for rp in self.closure:
            if rp.next is None:
                continue
            if rp.next.name not in checked:
                checked.add(rp.next.name)
                output.append(rp.next)
        return output

    def print (self):
        rows = []
        print('STATE(%d): %s'%(self.uuid, self.name))
        for i, rp in enumerate(self.closure):
            t = (i < len(self.kernel)) and '(K)' or '(C)'
            rows.append([' ', i, str(rp), '  ', t])
        text = cstring.tabulify(rows, 0)
        print(text)
        print()
        return 0


#----------------------------------------------------------------------
# ActionName
#----------------------------------------------------------------------
class ActionName (IntEnum):
    SHIFT = 0
    REDUCE = 1
    ACCEPT = 2
    ERROR = 3


#----------------------------------------------------------------------
# Action
#----------------------------------------------------------------------
class Action (object):
    
    def __init__ (self, name: int, target: int, rule: Production = None):
        self.name = name
        self.target = target
        self.rule = rule

    def __eq__ (self, obj):
        assert isinstance(obj, Action)
        return (self.name == obj.name and self.target == obj.target)

    def __ne__ (self, obj):
        return (not (self == obj))

    def __hash__ (self):
        return hash((self.name, self.target))

    def __ge__ (self, obj):
        return ((self.name, self.target) >= (obj.name, obj.target))

    def __gt__ (self, obj):
        return ((self.name, self.target) > (obj.name, obj.target))

    def __lt__ (self, obj):
        return (not (self > obj))

    def __le__ (self, obj):
        return (not (self >= obj))

    def __str__ (self):
        if self.name == ActionName.ACCEPT:
            return 'acc'
        if self.name == ActionName.ERROR:
            return 'err'
        name = ActionName(self.name).name[:1]
        return name + '/' + str(self.target)

    def __repr__ (self):
        return '%s(%r, %r)'%(type(self).__name__, self.mode, self.target)


#----------------------------------------------------------------------
# LRTable
#----------------------------------------------------------------------
class LRTable (object):

    def __init__ (self, head):
        self.head = self.__build_head(head)
        self.rows = []
        self.mode = 0

    def __build_head (self, head):
        terminal = []
        nonterminal = []
        for symbol in head:
            if symbol.term:
                if symbol.name != '$':
                    terminal.append(symbol)
            else:
                if symbol.name != 'S^':
                    nonterminal.append(symbol)
        terminal.sort()
        terminal.append(EOF)
        nonterminal.sort()
        # nonterminal.insert(0, Symbol('S^', False))
        output = terminal + nonterminal
        return tuple(output)

    def __len__ (self):
        return len(self.rows)

    def __getitem__ (self, row):
        return self.rows[row]

    def __contains__ (self, row):
        return ((row >= 0) and (row < len(self.rows)))

    def __iter__ (self):
        return self.rows.__iter__()

    def clear (self):
        self.rows.clear()
        return 0

    def get (self, row, col):
        if row not in self.rows:
            return None
        return self.rows[row].get(col, None)

    def set (self, row, col, data):
        if isinstance(col, Symbol):
            col = col.name
        if row >= len(self.rows):
            while row >= len(self.rows):
                self.rows.append({})
        rr = self.rows[row]
        if self.mode == 0:
            rr[col] = set([data])
        else:
            rr[col] = [data]
        return 0

    def add (self, row, col, data):
        if isinstance(col, Symbol):
            col = col.name
        if row >= len(self.rows):
            while row >= len(self.rows):
                self.rows.append({})
        rr = self.rows[row]
        if self.mode == 0:
            if col not in rr:
                rr[col] = set([])
            rr[col].add(data)
        else:
            if col not in rr:
                rr[col] = []
            rr[col].append(data)
        return 0

    def print (self):
        rows = []
        head = ['STATE'] + [str(n) for n in self.head]
        rows.append(head)
        for i, row in enumerate(self.rows):
            body = [str(i)]
            for n in self.head:
                col = n.name
                if col not in row:
                    body.append('')
                else:
                    p = row[col]
                    text = ','.join([str(x) for x in p])
                    body.append(text)
            rows.append(body)
        text = cstring.tabulify(rows, 1)
        print(text)
        return 0


#----------------------------------------------------------------------
# Node
#----------------------------------------------------------------------
class Node (object):
    
    def __init__ (self, name, child):
        self.name = name
        self.child = [n for n in child]

    def __repr__ (self):
        clsname = type(self).__name__
        return '%s(%r, %r)'%(clsname, self.name, self.child)

    def __str__ (self):
        return self.__repr__()

    def print (self, prefix = ''):
        print(prefix, end = '')
        print(self.name)
        for child in self.child:
            if isinstance(child, Node):
                child.print(prefix + '| ')
            elif isinstance(child, Symbol):
                print(prefix + '| ' + str(child))
            else:
                print(prefix + '| ' + str(child))
        return 0


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        p = Production('E', ['E', '"+"', 'T'])
        print(p)
        rp = RulePtr(p, 0)
        while rp:
            print(rp)
            al = rp.after_list()
            print(type(al), al)
            rp = rp.advance()
        return 0
    def test2():
        head = [Symbol('E'), Symbol("'d'"), Symbol('S'), Symbol("'c'")]
        tab = LRTable(head)
        a = Action(ActionName.SHIFT, 2)
        tab.set(5, "'d'", a)
        # print(tab.head)
        tab.print()
        return 0
    def test3():
        node: Node = Node(Symbol('E'), [Symbol("'d'"), Symbol("c")])
        print(node)
        node.print()
    test3()



