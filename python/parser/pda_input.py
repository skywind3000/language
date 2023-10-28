#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# pda_input.py - 
#
# Created by skywind on 2023/01/19
# Last Modified: 2023/01/19 09:33:22
#
#======================================================================
import sys
import re
import pprint

import console
import cstring
import internal
import grammar
import grammar_loader
import lrlib
import lalr

from enum import Enum

from grammar import Grammar, Symbol, Production, Vector, GrammarError
from grammar_analyzer import GrammarAnalyzer, SymbolInfo, EPSILON, EOF
from lalr import LALRItemSet, PSHARP
from lrlib import RulePtr, LRItemSet, Action, ActionName, LRTable
from lexer import LexerError, Lexer
from ctoken import Token


#----------------------------------------------------------------------
# export
#----------------------------------------------------------------------
__all__ = ['PushDownInput']


#----------------------------------------------------------------------
# push down automata input
#----------------------------------------------------------------------
class PushDownInput (object):

    def __init__ (self, g:Grammar):
        self.g: Grammar = g
        self.lexer: Lexer = None
        self.it = None
        self.eof: bool = False
        self.matcher = None

    def open (self, code):
        if isinstance(code, str):
            self.lexer = self.__open_lexer()
            self.it = self.lexer.tokenize(code)
        elif hasattr(code, 'read'):
            content = code.read()
            self.lexer = self.__open_lexer()
            self.it = self.lexer.tokenize(content)
        elif hasattr(code, '__iter__'):
            self.it = iter(code)
        else:
            raise TypeError('invalid source type')
        self.eof = False
        return 0

    def read (self):
        if self.eof:
            return None
        try:
            token = next(self.it)
        except StopIteration:
            if not self.eof:
                self.eof = True
                return Token('$', '', None, None)
            return None
        if token.name == '$':
            self.eof = True
        return token

    def __open_lexer (self):
        lexer: Lexer = Lexer()
        for scanner in self.g.scanner:
            cmd: str = scanner[0].strip()
            if cmd in ('ignore', 'skip'):
                lexer.push_skip(scanner[1])
            elif cmd == 'match':
                lexer.push_match(scanner[1], scanner[2])
            elif cmd == 'import':
                lexer.push_import_match(scanner[1], scanner[2])
        for terminal in self.g.terminal.values():
            if cstring.string_is_quoted(terminal.name):
                text = cstring.string_unquote(terminal.name)
                lexer.push_literal(text)
        lexer.register('*', self.__handle_lexer_action)
        # lexer.push_literal('if')
        return lexer

    def __handle_lexer_action (self, action, text):
        if self.matcher is not None:
            clsname = self.matcher.__class__.__name__
            if hasattr(self.matcher, action):
                func = getattr(self.matcher, action)
                return func(text)
            else:
                raise TypeError('method "%s" is undefined in %s'%(action, clsname))
        return (cstring.string_quote('{%s}'%action), text)


#----------------------------------------------------------------------
# match action
#----------------------------------------------------------------------
class MatchAction (object):
    def __init__ (self):
        action = {}
        for name, func in self.__class__.__dict__.items():
            if not name.startswith('__'):
                action[name] = func
        self._MATCH_ACTION_ = action


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        g = grammar_loader.load_from_file('grammar/test_lex1.txt')
        pi = PushDownInput(g)
        source = '''
        10 + 5 * 2 + x / 3
        if x > 0 then print(x)
        '''
        # pprint.pprint(pi.lexer.literal)
        class Matcher:
            def action1 (self, text):
                if text == 'if':
                    return "IF"
                elif text == 'then':
                    return 'THEN'
                return ('NAME', text)
        pi.matcher = Matcher()
        pi.open(source)
        # pi.lexer.push_literal(
        while 1:
            token = pi.read()
            if token is None:
                break
            print(token)
        return 0
    def test2():
        class MyClass(MatchAction):
            def foo(self):
                print('bar')
        o = MyClass()
        if hasattr(o, 'foo'):
            foo = getattr(o, 'foo')
            foo()
        print(o._MATCH_ACTION_)
        print(o.__class__.__name__)
        return 0
    test1()


