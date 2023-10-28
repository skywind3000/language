#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# pda.py - 
#
# Created by skywind on 2015/01/19
# Last Modified: 2023/01/19 06:16:59
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
from ctoken import Token
from lalr_analyzer import LALRAnalyzer


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['PDA']


#----------------------------------------------------------------------
# internal
#----------------------------------------------------------------------
LOG_DEBUG = lambda *args: console.log(internal.LOG_PDA_DEBUG, *args)
LOG_INFO = lambda *args: console.log(internal.LOG_PDA, *args)
LOG_ERROR = lambda *args: console.log(internal.LOG_ERROR, *args)


#----------------------------------------------------------------------
# LR Push Down Automata
#----------------------------------------------------------------------
class PDA (object):

    def __init__ (self, g:Grammar, tab:LRTable):
        self.g: Grammar = g
        self.tab: LRTable = tab
        self.input: PushDownInput = PushDownInput(self.g)
        self.state_stack = []
        self.symbol_stack = []
        self.value_stack = []
        self.current = None
        self._semantic_action = None
        self._lexer_action = None
        self._is_accepted = False
        self.accepted: bool = False
        self.error = None
        self.result = None
        self.debug = False
        self.filename = '<buffer>'

    def install_semantic_action (self, obj):
        self._semantic_action = obj
        return 0

    def install_lexer_action (self, obj):
        self._lexer_action = obj
        self.input.matcher = obj
        return 0

    def error_token (self, token:Token, *args):
        if not token.line:
            LOG_ERROR(*args)
        else:
            msg = 'error:%s:%d:'%(self.filename, token.line)
            LOG_ERROR(msg, *args)
        return 0

    def open (self, code):
        self.state_stack.clear()
        self.symbol_stack.clear()
        self.value_stack.clear()
        self.input.open(code)
        self.accepted: bool = False
        self.error = None
        self.state_stack.append(0)
        self.symbol_stack.append(EOF)
        self.value_stack.append(None)
        self.current: Token = self.input.read()
        self.result = None
        return 0

    def step (self):
        if self.accepted:
            return -1
        elif self.error:
            return -2
        if len(self.state_stack) <= 0:
            LOG_ERROR('PDA fatal error')
            assert len(self.state_stack) > 0
            return -3
        state: int = self.state_stack[-1]
        tab: LRTable = self.tab
        if state not in tab:
            LOG_ERROR('state %d does not in table')
            assert state in tab
            return -4
        lookahead: Token = self.current
        data = tab.rows[state].get(lookahead.name, None)
        if not data:
            self.error = 'unexpected token: %r'%lookahead
            self.error_token(lookahead, self.error)
            return -5
        action: Action = list(data)[0]
        if not action:
            self.error = 'invalid action'
            self.error_token(lookahead, 'invalid action:', str(action))
            return -6
        retval = 0
        if action.name == ActionName.SHIFT:
            symbol: Symbol = Symbol(lookahead.name, True)
            newstate: int = action.target
            self.state_stack.append(newstate)
            self.symbol_stack.append(symbol)
            self.value_stack.append(lookahead.value)
            self.current = self.input.read()
            if self.debug:
                print('action: shift/%d'%action.target)
            retval = 1
        elif action.name == ActionName.REDUCE:
            retval = self.__proceed_reduce(action.target)
        elif action.name == ActionName.ACCEPT:
            assert len(self.state_stack) == 2
            self.accepted = True
            self.result = self.value_stack[-1]
            if self.debug:
                print('action: accept')
        elif action.name == ActionName.ERROR:
            self.error = 'syntax error'
            if hasattr(action, 'text'):
                self.error += ': ' + getattr(action, 'text')
            self.error_token(lookahead, self.error)
            if self.debug:
                print('action: error')
            retval = -7
        if self.debug:
            print()
        return retval

    # stack: 0 1 2 3 4(top)
    def __generate_args (self, size):
        if len(self.state_stack) <= size:
            return None
        top = len(self.state_stack) - 1
        args = []
        for i in range(size + 1):
            args.append(self.value_stack[top - size + i])
        return args

    def __execute_action (self, rule: Production, actname: str, actsize: int):
        value = None
        args = self.__generate_args(actsize)
        if not self._semantic_action:
            return 0, None
        name = actname
        if name.startswith('{') and name.endswith('}'):
            name = name[1:-1].strip()
        callback = self._semantic_action
        if not hasattr(callback, name):
            raise KeyError('action %s is not defined'%actname)
        func = getattr(callback, name)
        value = func(rule, args)
        return 1, value

    def __rule_eval (self, rule: Production):
        parent: Production = rule
        if hasattr(rule, 'parent'):
            parent: Production = rule.parent
        size = len(rule.body)
        if len(self.state_stack) <= size:
            self.error = 'stack size is not enough'
            raise ValueError('stack size is not enough')
        value = None
        executed = 0
        action_dict = rule.action and rule.action or {}
        for pos, actions in action_dict.items():
            if pos != size:
                LOG_ERROR('invalid action pos: %d'%pos)
                continue
            for action in actions:
                if isinstance(action, str):
                    actname = action
                    actsize = size
                elif isinstance(action, tuple):
                    actname = action[0]
                    actsize = action[1]
                else:
                    LOG_ERROR('invalid action type')
                    continue
                hr, vv = self.__execute_action(parent, actname, actsize)
                if hr > 0:
                    value = vv
                    executed += 1
        if executed == 0:
            args = self.__generate_args(size)
            value = Node(rule.head, args[1:])
        return value

    def __proceed_reduce (self, target: int):
        rule: Production = self.g.production[target]
        size = len(rule.body)
        value = self.__rule_eval(rule)
        for i in range(size):
            self.state_stack.pop()
            self.symbol_stack.pop()
            self.value_stack.pop()
        assert len(self.state_stack) > 0
        top = len(self.state_stack) - 1  # noqa
        state = self.state_stack[-1]
        tab: LRTable = self.tab
        if state not in tab:
            LOG_ERROR('state %d does not in table')
            assert state in tab
            return -10
        data = tab.rows[state].get(rule.head.name, None)
        if not data:
            self.error = 'reduction state mismatch'
            self.error_token(self.current, self.error)
            return -11
        action: Action = list(data)[0]
        if not action:
            self.error = 'invalid action: %s'%str(action)
            self.error_token(self.current, self.error)
            return -12
        if action.name != ActionName.SHIFT:
            self.error = 'invalid action name: %s'%str(action)
            self.error_token(self.current, self.error)
            return -13
        newstate = action.target
        self.state_stack.append(newstate)
        self.symbol_stack.append(rule.head)
        self.value_stack.append(value)
        if self.debug:
            print('action: reduce/%d -> %s'%(target, rule))
        return 0

    def is_stopped (self) -> bool:
        if self.accepted:
            return True
        if self.error:
            return True
        return False

    def run (self):
        while not self.is_stopped():
            if self.debug:
                self.print()
            self.step()
        return self.result

    def print (self):
        print('stack:', self.state_stack)
        text = '['
        symbols = []
        for n in self.symbol_stack:
            symbols.append(str(n))
        text += (', '.join(symbols)) + ']'
        print('symbol:', text)
        print('lookahead:', self.current and self.current.name or None)
        return 0



#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/expr_bnf.txt')
        la = LALRAnalyzer(g)
        la.process()
        tab: LRTable = la.tab
        tab.print()
        print()
        pda = PDA(g, tab)
        pda.open('1+2*3')
        while not pda.is_stopped():
            pda.step()
        print(pda.accepted)
        pprint.pprint(pda.result)
        return 0
    def test2():
        g = grammar_loader.load_from_file('grammar/expr_action.txt')
        la = LALRAnalyzer(g)
        la.process()
        tab: LRTable = la.tab
        tab.print()
        print()
        pda = PDA(g, tab)
        class actions:
            def add (self, rule, args):
                return args[1] + args[3]
            def sub (self, rule, args):
                return args[1] - args[3]
            def mul (self, rule, args):
                return args[1] * args[3]
            def div (self, rule, args):
                return args[1] * args[3]
            def get1 (self, rule, args):
                return args[1]
            def get2 (self, rule, args):
                return args[2]
            def getint (self, rule, args):
                return int(args[1])
        pda.install_semantic_action(actions())
        pda.open('1+2*3+(5-2)*2')
        pda.debug = 1
        pda.run()
        print(pda.accepted)
        pprint.pprint(pda.result)
        return 0
    test2()



