#! /usr/bin/env python
# -*- coding: utf-8 -*-
#======================================================================
#
# lr_conflict.py - 
#
# Created by skywind on 2023/01/25
# Last Modified: 2023/01/25 13:32:13
#
#======================================================================
import sys
import re
import pprint

import cstring
import console
import internal
import grammar
import grammar_loader
import lrlib
import lalr
import lexer

from enum import Enum, IntEnum

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF
from lalr import LALRItemSet, PSHARP
from lrlib import RulePtr, LRItemSet, Action, ActionName, LRTable, Node
from lexer import LexerError, Lexer
from pda_input import PushDownInput
from lalr_analyzer import LALRAnalyzer


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



#----------------------------------------------------------------------
# conflict solver
#----------------------------------------------------------------------
class ConflictSolver (object):

    def __init__ (self, g:Grammar, tab: LRTable):
        self.g: Grammar = g
        self.tab: LRTable = tab
        self.conflicted = 0
        self.state = -1

    def error_rule (self, rule:Production, text:str):
        anchor = self.g.anchor_get(rule)
        if anchor is None:
            LOG_ERROR('error: %s'%text)
            return 0
        LOG_ERROR('error:%s:%d: %s'%(anchor[0], anchor[1], text))
        return 0

    def warning_rule (self, rule:Production, text:str):
        anchor = self.g.anchor_get(rule)
        if anchor is None:
            LOG_ERROR('warning: %s'%text)
            return 0
        LOG_ERROR('warning:%s:%d: %s'%(anchor[0], anchor[1], text))
        return 0

    def _conflict_type (self, action1:Action, action2:Action):
        if action1.name == ActionName.SHIFT:
            if action2.name == ActionName.SHIFT:
                return 'shift/shift'
            elif action2.name == ActionName.REDUCE:
                return 'shift/reduce'
        elif action1.name == ActionName.REDUCE:
            if action2.name == ActionName.SHIFT:
                return 'shift/reduce'
            elif action2.name == ActionName.REDUCE:
                return 'reduce/reduce'
        n1 = ActionName(action1.name)
        n2 = ActionName(action2.name)
        return '%s/%s'%(n1.name, n2.name)

    def process (self):
        tab: LRTable = self.tab
        self.state = -1
        for row in tab.rows:
            self.state += 1
            for key in list(row.keys()):
                cell = row[key]
                if not cell:
                    continue
                if len(cell) <= 1:
                    continue
                self._solve_conflict(cell)
        return 0

    def _solve_conflict (self, actionset):
        if not actionset:
            return 0
        if len(actionset) <= 1:
            return 0
        final = None
        for action in actionset:
            if final is None:
                final = action
                continue
            final = self._compare_rule(final, action)
        if isinstance(actionset, set):
            actionset.clear()
            actionset.add(final)
        elif isinstance(actionset, list):
            actionset.clear()
            actionset.push(final)
        return 0

    def warning_conflict (self, action1:Action, action2:Action):
        rule1:Production = action1.rule
        rule2:Production = action2.rule
        ctype:str = self._conflict_type(action1, action2)
        text = 'conflict %d %s with'%(self.state, ctype)
        text += ' ' + str(rule2)
        self.warning_rule(rule1, text)

    def _pick_shift (self, action1:Action, action2:Action, warning = False):
        if action1.name == ActionName.SHIFT:
            if warning:
                self.warning_rule(action2.rule, 'discard rule: %s'%str(action2.rule))
            return action1
        elif action2.name == ActionName.SHIFT:
            if warning:
                self.warning_rule(action2.rule, 'discard rule: %s'%str(action1.rule))
            return action2
        return action1

    def _pick_reduce (self, action1:Action, action2:Action, warning = False):
        if action1.name == ActionName.REDUCE:
            if warning:
                self.warning_rule(action2.rule, 'discard rule: %s'%str(action2.rule))
            return action1
        elif action2.name == ActionName.REDUCE:
            if warning:
                self.warning_rule(action2.rule, 'discard rule: %s'%str(action1.rule))
            return action2
        return action1

    def _compare_rule (self, action1:Action, action2:Action):
        rule1:Production = action1.rule
        rule2:Production = action2.rule
        if rule1.precedence is None:
            self.warning_conflict(action1, action2)
            if rule2.precedence is None:
                return self._pick_shift(action1, action2, True)
            return action2
        elif rule2.precedence is None:
            self.warning_conflict(action1, action2)
            return action1
        n1 = rule1.precedence
        n2 = rule2.precedence
        if n1 not in self.g.precedence:
            self.warning_rule(rule1, 'precedence %s not defined'%n1)
            return action1
        if n2 not in self.g.precedence:
            self.warning_rule(rule2, 'precedence %s not defined'%n2)
            return action1
        p1:int = self.g.precedence[n1]
        p2:int = self.g.precedence[n2]
        if p1 > p2:
            return action1
        elif p1 < p2:
            return action2
        a1:str = self.g.assoc[n1]
        a2:str = self.g.assoc[n2]
        if a1 == 'left':
            return self._pick_reduce(action1, action2)
        elif a1 == 'right':
            return self._pick_shift(action1, action2)
        return action1


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/conflict_2.txt')
        lalr = LALRAnalyzer(g)
        hr = lalr.process()
        if hr != 0:
            print('bad grammar')
            return 1
        lalr.tab.print()
        print()
        cs = ConflictSolver(g, lalr.tab)
        cs.process()
        lalr.tab.print()
        return 0
    test1()




