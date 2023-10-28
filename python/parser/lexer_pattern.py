#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lexer_pattern.py - 
#
# Created by skywind on 2023/01/19
# Last Modified: 2023/01/19 04:51
#
#======================================================================
import pprint
import ctoken
import ctoken_regex


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['PATTERN']


#----------------------------------------------------------------------
# rules
#----------------------------------------------------------------------
lex_rules = r'''
    O = [0-7]
    D = [0-9]
    NZ = [1-9]
    L = [a-zA-Z_]
    A = [a-zA-Z_0-9]
    H = [a-fA-F0-9]
    HP = (0[xX])
    E = ([Ee][+-]?{D}+)
    P = ([Pp][+-]?{D}+)
    FS = (f|F|l|L)
    IS = (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
    CP = (u|U|L)
    SP = (u8|u|U|L)

    WHITESPACE = \s+
    WS = \s+
    EOL = [\n]+
    WSEOL = {WS}|{EOL}
    COMMENT1 = [#].*
    COMMENT2 = \/\/.*
    COMMENT3 = \/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+\/
    COMMENT = {COMMENT1}|{COMMENT2}|{COMMENT3}
    NAME = {L}{A}*
    STRING1 = '(?:\\.|[^'\\])*'
    STRING2 = "(?:\\.|[^"\\])*"
    STRING = {STRING1}|{STRING2}
    DIGIT = [0-9]
    DIGITS = {DIGIT}+
    HEX = {HP}{H}+
    DEC = {NZ}{D}*
    INTEGER = ({HEX}|{DEC})(({IS}|{CP})?)
    FLOAT = {DIGITS}((\.{DIGITS})?)({E}?)({FS}?)
    NUMBER = {INTEGER}|{FLOAT}
'''


#----------------------------------------------------------------------
# build
#----------------------------------------------------------------------
PATTERN = ctoken_regex.regex_build(lex_rules, capture = False)


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        pprint.pprint(PATTERN)
        return 0
    test1()

