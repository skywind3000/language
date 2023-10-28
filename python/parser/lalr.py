#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lalr.py - 
#
# Created by skywind on 2015/01/13
# Last Modified: 2023/01/13 01:37:25
#
#======================================================================
import sys
import copy
import console
import cstring
import internal
import grammar
import lrlib

from enum import Enum

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF
from lrlib import RulePtr, LRItemSet, Action, ActionName


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['LALRItemSet', 'PSHARP']


#----------------------------------------------------------------------
# for analyzing propaganda
#----------------------------------------------------------------------
PSHARP = Symbol('#', True)


#----------------------------------------------------------------------
# internal
#----------------------------------------------------------------------
LOG_DEBUG = lambda *args: console.log(internal.LOG_LR_DEBUG, *args)
LOG_INFO = lambda *args: console.log(internal.LOG_LR, *args)
LOG_ERROR = lambda *args: console.log(internal.LOG_ERROR, *args)


#----------------------------------------------------------------------
# LALRItemSet
#----------------------------------------------------------------------
class LALRItemSet (LRItemSet):

    def __init__ (self, kernel_source):
        super(LALRItemSet, self).__init__(kernel_source)
        self.lookahead = [set([]) for n in range(len(self.kernel))]
        self.dirty = False

    def shrink (self):
        while len(self.closure) > len(self.kernel):
            self.closure.pop()
        return 0

    def print (self):
        rows = []
        print('STATE(%d): %s'%(self.uuid, self.name))
        for i, rp in enumerate(self.closure):
            if i < len(self.kernel):
                p = ' ' .join([str(n) for n in self.lookahead[i]])
                p = '{' + p + '}'
                t = '(K)'
            else:
                p = ''
                t = '(C)'
            rows.append([' ', i, str(rp), p, '  ', t])
        text = cstring.tabulify(rows, 0)
        print(text)
        print()
        return 0


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        p = Production('E', ['E', '"+"', 'T'])
        print(p)
        li = RulePtr(p, 0)
        while li:
            print(li)
            print(type(li))
            al = li.after_list()
            print(type(al), al)
            li = li.advance()
        return 0
    test1()


