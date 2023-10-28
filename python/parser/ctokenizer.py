#! /usr/bin/env python
# -*- coding: utf-8 -*-
#======================================================================
#
# ctokenizer.py - tokenizer basic
#
# Created by skywind on 2022/12/26
# Last Modified: 2022/12/26 15:39:27
#
#======================================================================
from __future__ import print_function, unicode_literals
import sys
import io
import ctoken

from ctoken_type import *


#----------------------------------------------------------------------
# tokenizer
#----------------------------------------------------------------------
class Tokenizer (object):

    def __init__ (self, code):
        if isinstance(code, str):
            self.fp = io.StringIO(code)
        elif isinstance(code, bytes):
            self.fp = io.StringIO(code.decode())
        else:
            self.fp = code
        self.prev = None
        self.ch = None
        self.next = None
        self.line = 1
        self.column = 0
        self.position = 0
        self.keywords = {}
        self.operators = {}
        self.specials = {}
        self.__raw_line = 1
        self.__raw_column = 1
        self.__raw_position = 0
        self.__p_cur = None
        self.__p_next = None
        self._line_stop = {}
        self.eof = False
        self.error = ''

    def __raw_getch (self):
        ch = self.fp.read(1)
        hr = (ch, self.__raw_line, self.__raw_column, self.__raw_position)
        if ch == '\n':
            self._line_stop[self.__raw_line] = self.__raw_column
            self.__raw_column = 1
            self.__raw_line += 1
        elif ch != '':
            self.__raw_column += 1
        self.__raw_position += 1
        return hr

    def getch (self):
        if self.__p_next is None:
            self.__p_next = self.__raw_getch()
        self.prev = self.ch
        self.__p_cur = self.__p_next
        self.__p_next = self.__raw_getch()
        self.ch, self.line, self.column, self.position = self.__p_cur
        self.next = self.__p_next[0]
        if self.ch == '':
            self.eof = True
        return self.ch

    def generate (self, name, text, line, column):
        if text in self.keywords:
            i = self.keywords[text]
            name = (i >= CTOKEN_OTHER) and i or CTOKEN_KEYWORD
        elif text in self.operators:
            i = self.operators[text]
            name = (i >= CTOKEN_OTHER) and i or CTOKEN_OPERATOR
        elif text in self.specials:
            i = self.specials[text]
            name = (i >= CTOKEN_OTHER) and i or CTOKEN_SPECIAL
        return ctoken.Token(name, text, line, column)

    def __next__ (self):
        token = self.read()
        if token is None:
            raise StopIteration
        return token

    def __iter__ (self):
        return self

    def read (self):
        return None


#----------------------------------------------------------------------
# c tokenizer
#----------------------------------------------------------------------
class CTokenizer (Tokenizer):

    def __init__ (self, code):
        super(CTokenizer, self).__init__(code)
        self.__setup_c_keywords()
        self.__setup_c_operators()
        self.__setup_c_specials()
        self._has_eof = False
        self._prev_line = 1
        self._buffer = []
        self.emit_eol = True

    def __setup_c_keywords (self):
        keywords = (
            'auto', 'break', 'case', 'char', 'const', 'continue', 'default',
            'define', 'do', 'double', 'elif', 'else', 'endif', 'enum',
            'error', 'extern', 'float', 'for', 'goto', 'if', 'ifdef',
            'ifndef', 'include', 'inline', 'int', 'line', 'long', 'noalias',
            'pragma', 'register', 'restrict', 'return', 'short', 'signed',
            'sizeof', 'static', 'struct', 'switch', 'typedef', 'undef',
            'union', 'unsigned', 'void', 'volatile', 'while', )
        for kw in keywords:
            self.keywords[kw] = 0
        return 0

    def __setup_c_operators (self):
        operators = (
            '++', '--', '.', '->', '~', '!', '+', '-', '&', '_Alignof',
            'sizeof', '?:', ',', '*', '/', '%', '<<', '>>', '<', '>', '<=',
            '>=', '==', '!=', '^', '|', '&&', '||', '=', '*=', '/=', '%=',
            '+=', '-=', '>>=', '<<=', '&=', '^=', '|=', '?', ':', )
        for op in operators:
            self.operators[op] = 0
        return 0

    def __setup_c_specials (self):
        specials = ('(', '[', '{', '}', ']', ')', '#')
        for sp in specials:
            self.specials[sp] = 0
        return 0

    def _skip_space (self):
        skip = 1
        while 1:
            if self.ch == '':
                return -1
            if self.ch not in (' ', '\r', '\n', '\t'):
                break
            self.getch()
            skip += 1
        return skip

    def _skip_memo (self):
        skip = 0
        while 1:
            self._skip_space()
            if self.ch == '':
                break
            if self.ch == '/' and self.next == '/':
                skip += 2
                while self.ch != '\n' and self.ch != '':
                    self.getch()
                    skip += 1
            elif self.ch == '/' and self.next == '*':
                self.getch()
                self.getch()
                skip += 2
                while self.ch != '':
                    if self.ch == '*' and self.next == '/':
                        self.getch()
                        self.getch()
                        skip += 2
                        break
                    else:
                        skip += 1
                        self.getch()
            else:
                break
        return skip

    def _read_string (self):
        self.error = ''
        if self.ch not in ('"', "'"):
            return None
        mode = (self.ch == '\'') and 1 or 0
        text = self.ch
        while 1:
            ch = self.getch()
            if ch == '\\':
                text += ch + self.next
                ch = self.getch()
            elif (mode == 0) and (ch == '\''):
                text += ch
            elif (mode == 1) and (ch == '\"'):
                text += ch
            elif (mode == 0) and (ch == '\"'):
                text += ch
                self.getch()
                break
            elif (mode == 1) and (ch == '\''):
                text += ch
                self.getch()
                break
            elif ch == '\n':
                self.error = 'EOL while scanning string literal'
                self.code = 1
            elif ch != '':
                text += ch
            else:
                self.error = 'EOF while scanning string literal'
                self.code = 2
                break
        return text

    def _read_operator_sp (self, chset):
        if self.ch not in chset:
            return ''
        if self.ch + self.next in chset:
            text = self.ch + self.next
            self.getch()
            self.getch()
            while text + self.ch in chset:
                text += self.ch
                self.getch()
                if self.ch == '':
                    break
        else:
            text = self.ch
            self.getch()
        return text

    def _read_number (self):
        if ((self.ch < '0') or (self.ch > '9')):
            return None
        text = ''
        while self.ch.isalnum() or self.ch == '.':
            text += self.ch
            self.getch()

        pos = len(text)
        while pos > 0:
            ch = text[pos - 1]
            if ch.isdigit() or ch == '.':
                break
            if ch >= 'A' and ch <= 'F':
                break
            if ch >= 'a' and ch <= 'f':
                break
            pos -= 1
        if len(text) - pos > 2:
            self.error = 'number format error'
            self.code = 1
            return None

        return text

    def _low_read (self):
        if self.ch is None:
            self.getch()
        self._skip_memo()
        if self.eof:
            if not self._has_eof:
                self._has_eof = True
                return self.generate(CTOKEN_EOF, None, self.line, self.column)
            return None
        # read identifier or keyword
        if self.ch.isalpha() or self.ch == '_':
            word = ''
            line = self.line
            column = self.column
            while self.ch.isalnum() or self.ch == '_':
                word += self.ch
                self.getch()
                if self.ch == '':
                    break
            return self.generate(CTOKEN_NAME, word, line, column)
        # read string
        if self.ch in ('"', "'"):
            line, column = self.line, self.column
            text = self._read_string()
            return self.generate(CTOKEN_STRING, text, line, column)
        # read operators
        if self.ch in self.operators:
            line, column = self.line, self.column
            text = self._read_operator_sp(self.operators)
            return self.generate(CTOKEN_OPERATOR, text, line, column)
        # read special symbol
        if self.ch in self.specials:
            line, column = self.line, self.column
            text = self._read_operator_sp(self.specials)
            return self.generate(CTOKEN_SPECIAL, text, line, column)
        # read number
        if self.ch >= '0' and self.ch <= '9':
            line, column = self.line, self.column
            text = self._read_number()
            if text is None:
                self.error = 'bad number: ' + text
                return None
            return self.generate(CTOKEN_NUMBER, text, line, column)
        if 1:
            line, column = self.line, self.column
            ch = self.ch
            self.getch()
            return self.generate(CTOKEN_SPECIAL, ch, line, column)
        self.error = 'unexpected character: %s at line %d'%(self.ch, self.line)
        return None

    def read (self):
        if not self._buffer:
            token = self._low_read()
            if token and token.line != self._prev_line:
                ends = self._line_stop.get(self._prev_line, 0)
                eol = ctoken.Token(CTOKEN_ENDLINE, '\\n', self._prev_line, ends)
                self._prev_line = token.line
                if self.emit_eol:
                    self._buffer.append(eol)
            self._buffer.append(token)
        token = self._buffer[0]
        self._buffer = self._buffer[1:]
        return token


#----------------------------------------------------------------------
# pygments
#----------------------------------------------------------------------
def pygments_tokenizer(source, lang = 'cpp'):
    import pygments
    from pygments import token
    from pygments.lexers import get_lexer_by_name
    lexer = get_lexer_by_name(lang)
    tokens = lexer.get_tokens(source)
    output = []
    pos = 0
    while pos < len(tokens):
        cur = tokens[pos]
        cur = cur
    output.append(ctoken.Token(CTOKEN_EOF, None, 0, 0))
    return output


#----------------------------------------------------------------------
# returns 
#----------------------------------------------------------------------
def simple_token(code):
    tokens = []
    for token in CTokenizer(code):
        # print(token)
        if token.name == CTOKEN_NUMBER:
            text = token.value
            if '.' in text:
                tokens.append(float(text))
            else:
                tokens.append(int(text, 0))
        elif token.name in (CTOKEN_KEYWORD, CTOKEN_NAME, CTOKEN_OPERATOR):
            tokens.append(token.value)
        elif token.name in (CTOKEN_SPECIAL, CTOKEN_STRING):
            tokens.append(token.value)
    return tokens
    



#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        for text in open('../test_tok.c'):
            print(text.rstrip())
        return 0
    def test2():
        for text in open('../test_tok.c'):
            print(text.rstrip())
        ct = CTokenizer(open('../test_tok.c'))
        for token in ct:
            print(token,)   
            if not token:
                print('ERROR: ', ct.error)
                break
            elif token.name == CTOKEN_EOF:
                break
    def test3():
        # tokens = simple_token(open('../test_tok.c'))
        # print(tokens)
        print(simple_token('x + 10   * 3.1415926 * (1+3)'))
    test3()


