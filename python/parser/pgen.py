#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# pgen.py - parser generator
#
# Created by skywind on 2023/01/25
# Last Modified: 2023/01/25 13:56:00
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
from lr1_analyzer import LR1Analyzer
from pda import PDA
from lr_conflict import ConflictSolver


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['Parser', 'create_parser', 'create_parser_from_file']



#----------------------------------------------------------------------
# create parser
#----------------------------------------------------------------------
class Parser (object):

    def __init__ (self, g:Grammar, tab:LRTable):
        self.g:Grammar = g
        self.tab:LRTable = tab
        self.pda:PDA = PDA(g, tab)
        self.error = None

    def __call__ (self, code, debug = False):
        self.error = None
        self.pda.open(code)
        self.pda.debug = debug
        self.pda.run()
        if self.pda.accepted:
            return self.pda.result
        self.error = self.pda.error
        return None



#----------------------------------------------------------------------
# create parser with grammar
#----------------------------------------------------------------------
def __create_with_grammar(g:Grammar, semantic_action, 
                          lexer_action, algorithm):
    if g is None:
        return None
    if algorithm.lower() in ('lr1', 'lr(1)'):
        algorithm = 'lr1'
    elif algorithm.lower() in ('lalr', 'lalr1', 'lalr(1)'):
        algorithm = 'lalr'
    else:
        algorithm = 'lr1'
    if algorithm == 'lr1':
        analyzer = LR1Analyzer(g)
    else:
        analyzer = LALRAnalyzer(g)
    hr = analyzer.process()
    if hr != 0:
        return None
    tab:LRTable = analyzer.tab
    cs:ConflictSolver = ConflictSolver(g, tab)
    cs.process()
    parser = Parser(g, tab)
    if semantic_action:
        parser.pda.install_semantic_action(semantic_action)
    if lexer_action:
        parser.pda.install_lexer_action(lexer_action)
    return parser


#----------------------------------------------------------------------
# create_parser
#----------------------------------------------------------------------
def create_parser(grammar_bnf: str, 
                  semantic_action = None,
                  lexer_action = None,
                  algorithm = 'lr1'):
    g = grammar_loader.load_from_string(grammar_bnf)
    return __create_with_grammar(g, semantic_action, lexer_action, algorithm)


#----------------------------------------------------------------------
# create from file
#----------------------------------------------------------------------
def create_parser_from_file(grammar_file_name: str,
                            semantic_action = None,
                            lexer_action = None,
                            algorithm = 'lr1'):
    g = grammar_loader.load_from_file(grammar_file_name)
    return __create_with_grammar(g, semantic_action, lexer_action, algorithm)


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        class MyAction:
            def add (self, rule, args):
                return args[1] + args[3]
            def sub (self, rule, args):
                return args[1] - args[3]
            def mul (self, rule, args):
                return args[1] * args[3]
            def div (self, rule, args):
                return args[1] / args[3]
            def get2 (self, rule, args):
                return args[2]
            def negative (self, rule, args):
                return -(args[2])
            def getint (self, rule, args):
                return int(args[1])
        parser = create_parser_from_file('grammar/conflict_2.txt', MyAction(), None)
        obj = parser('2+3*4*-2+2*(2+3)*2', debug = 1)
        print(obj)
        return 0
    test1()


