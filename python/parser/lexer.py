#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# lexer.py - 
#
# Created by skywind on 2023/01/14
# Last Modified: 2023/01/14 06:33:41
#
#======================================================================
import sys
import re

import ctoken
import ctoken_regex
import cstring

from ctoken import Token
from ctoken_regex import _tokenize
from lexer_pattern import PATTERN


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['Lexer', 'LexerError']


#----------------------------------------------------------------------
# LexerError
#----------------------------------------------------------------------
class LexerError (Exception):
    def __init__ (self, error, line, column):
        super(LexerError, self).__init__(error)
        self.line = line
        self.column = column


#----------------------------------------------------------------------
# lexer
#----------------------------------------------------------------------
class Lexer (object):

    def __init__ (self):
        self.rules = []
        self.literal = {}
        self.actions = {}
        self.require = {}
        self.intercept_literal = True

    def clear (self):
        self.rules.clear()
        self.literal.clear()
        self.require.clear()
        return 0

    def push_skip (self, pattern:str):
        self.rules.append((None, pattern))
        return 0

    def _is_action (self, name:str) -> bool:
        return (name.startswith('{') and name.endswith('}'))

    def push_match (self, name:str, pattern:str):
        name = name.strip()
        if self._is_action(name):
            action = name[1:-1].strip('\r\n\t ')
            self.require[action] = len(self.rules)
            self.rules.append((self.__handle_action, pattern, action))
        else:
            self.rules.append((name, pattern))
        return 0

    def push_import_match (self, name:str, key:str):
        # print('import', name)
        key = key.strip()
        if name is None:
            name = key
        name = name.strip()
        if key not in PATTERN:
            raise LexerError('%s cannot be imported'%key, None, None)
        return self.push_match(name, PATTERN[key])

    def push_import_skip (self, key:str):
        if key not in PATTERN:
            raise LexerError('%s cannot be imported'%key, None, None)
        return self.push_skip(PATTERN[key])

    def push_literal (self, literal:str):
        self.literal[literal] = cstring.string_quote(literal)

    def __handle_action (self, text:str, action:str):
        if text in self.literal:
            if self.intercept_literal:
                return (self.literal[text], text)
        if action in self.actions:
            fn = self.actions[action]
            return fn(action, text)
        if '*' in self.actions:
            fn = self.actions['*']
            return fn(action, text)
        raise LexerError('missing action {%s}'%action, None, None)

    def __handle_literal (self, text:str):
        quoted = cstring.string_quote(text, True)
        return (quoted, text)

    def register (self, action:str, callback):
        self.actions[action] = callback
        return 0

    def __convert_to_literal (self, token):
        if cstring.string_is_quoted(token.name):
            return token
        name = token.value
        if not isinstance(name, str):
            return token
        if name not in self.literal:
            return token
        t = Token(self.literal[name], name, token.line, token.column)
        return t

    def tokenize (self, code):
        rules = [n for n in self.rules]
        for literal in self.literal:
            escape = re.escape(literal)
            rules.append((self.__handle_literal, escape))
        rules.append((self.__handle_mismatch, '.'))
        last_line = 1
        last_column = 1
        try:
            for token in ctoken_regex.tokenize(code, rules, '$'):
                last_line = token.line
                last_column = token.column
                if isinstance(token.value, str):
                    if not cstring.string_is_quoted(token.name):
                        if token.value in self.literal:
                            if self.intercept_literal:
                                token = self.__convert_to_literal(token)
                yield token
        except LexerError as e:
            e.line = last_line
            e.column = last_column
            raise e
        return 0

    def __handle_mismatch (self, text):
        return (cstring.string_quote(text), text)


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        lexer = Lexer()
        lexer.push_import_skip('COMMENT')
        lexer.push_import_skip('WHITESPACE')
        lexer.push_import_skip('EOL')
        lexer.push_import_match('NUMBER', 'NUMBER')
        lexer.push_import_match('NAME', 'NAME')
        for ch in '+-*/':
            lexer.push_literal(ch)
        for kw in ('if', 'return', 'void', 'int'):
            lexer.push_literal(kw)
        source = '''
            // main function
            int main(void) {
                return 10 * 2 + 3;
            }
        '''
        source = '\n'.join([n.strip() for n in source.split('\n')])
        for token in lexer.tokenize(source):
            print(repr(token))
        return 0
    test1()




