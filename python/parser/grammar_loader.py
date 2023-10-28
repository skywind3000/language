#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# grammar_load.py - 
#
# Created by skywind on 2015/01/07
# Last Modified: 2023/01/10 00:44
#
#======================================================================
import sys
import os
import re
import grammar
import cstring
import internal
import ctoken
import ctoken_regex as ctr

from grammar import Symbol, Production, Grammar, GrammarError
from lexer_pattern import PATTERN


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['load_from_file', 'load_from_string']



#----------------------------------------------------------------------
# lex analyze
#----------------------------------------------------------------------
class GrammarLex (object):

    def __init__ (self):
        self.specific = self._build_pattern()

    def _build_pattern (self):
        spec = [
                    (None, ctr.PATTERN_COMMENT1),       # ignore
                    (None, ctr.PATTERN_COMMENT2),       # ignore
                    (None, ctr.PATTERN_COMMENT3),       # ignore
                    (None, ctr.PATTERN_WHITESPACE),     # ignore
                    (self._handle_string, ctr.PATTERN_STRING1),
                    (self._handle_string, ctr.PATTERN_STRING2),
                    (self._handle_macro, ctr.PATTERN_GMACRO),
                    (self._handle_integer, ctr.PATTERN_CINTEGER),
                    (self._handle_float, ctr.PATTERN_NUMBER),
                    ('BAR', r'\|'),
                    ('END', r'[;]'),
                    (':', r'[:]'),
                    ('LEX', r'[@].*'),
                    ('NAME', ctr.PATTERN_GNAME),
                    ('NAME', ctr.PATTERN_NAME),
                    (self._handle_action, r'\{[^\{\}]*\}'),
                    (None, r'\%\%'),
                    ('OPERATOR', r'[\+\-\*\/\?]'),
                    ('MISMATCH', r'.'),
                ]
        return spec

    def _handle_string (self, value):
        text = cstring.quoted_normalize(value)
        return ('STRING', text)

    def _handle_integer (self, value):
        return ('NUMBER', cstring.string_to_int(value))

    def _handle_float (self, value):
        if '.' not in value:
            return self._handle_integer(value)
        return ('NUMBER', cstring.string_to_float(value))

    def _handle_macro (self, value):
        value = value.strip('\r\n\t ').replace(' ', '')
        return ('MACRO', value)

    def _handle_action (self, value):
        value = value.strip('\r\n\t ')
        return ('ACTION', value)

    def process (self, source):
        tokens = {}
        for token in ctr.tokenize(source, self.specific):
            # print(repr(token))
            line_num = token.line
            if line_num not in tokens:
                tokens[line_num] = []
            tokens[line_num].append(token)
            # print(token)
        return tokens


#----------------------------------------------------------------------
# load grammar file
#----------------------------------------------------------------------
class GrammarLoader (object):

    def __init__ (self):
        self.line_num = 0
        self.file_name = ''
        self.code = 0
        self.source = ''
        self.precedence = 0
        self.srcinfo = {}
        self.lex = GrammarLex()

    def error (self, *args):
        text = ' '.join(args)
        fn = (not self.file_name) and '<buffer>' or self.file_name
        internal.echo_error(text, fn, self.line_num)
        return 0

    def error_token (self, token, text = None):
        fn = (not self.file_name) and '<buffer>' or self.file_name
        if text is None:
            text = 'unexpected token: %r'%(token.value, )
        internal.echo_error(text, fn, token.line, token.column)
        return 0

    def load (self, source, file_name = ''):
        if isinstance(source, str):
            self.source = source
        else:
            self.source = source.read()
            if isinstance(self.code, bytes):
                self.code = self.code.decode('utf-8', 'ignore')
        self.file_name = file_name and file_name or '<buffer>'
        hr = self._scan_grammar()
        if hr != 0:
            print('loading grammar failed %d'%hr)
            return None
        return self.g

    def load_from_file (self, file_name, encoding = None):
        if not os.path.exists(file_name):
            raise FileNotFoundError('No such file: %r'%file_name)
        self.source = cstring.load_file_text(file_name, encoding)
        self.file_name = file_name
        hr = self._scan_grammar()
        if hr != 0:
            print('loading grammar failed %d'%hr)
            return None
        return self.g

    def _scan_grammar (self):
        self.g = Grammar()
        self.g.start = None
        self.current_symbol = None
        self.line_num = 0
        self.precedence = 0
        self._cache = []
        self.srcinfo.clear()
        tokens = self.lex.process(self.source)
        keys = list(tokens.keys())
        keys.sort()
        for line_num in keys:
            self.line_num = line_num
            args = tokens[line_num]
            hr = 0
            if not args:
                continue
            if args[0].name == 'MACRO':
                hr = self._process_macro(args)
            elif args[0].name == 'LEX':
                hr = self._process_lexer(args)
            else:
                hr = self._process_grammar(args)
            if hr != 0:
                return hr
        if len(self._cache) > 0:
            self._process_rule(self._cache)
        self.g.update()
        if self.g.start is None:
            if len(self.g.production) > 0:
                self.g.start = self.g.production[0].head
        if self.g.start:
            symbol = self.g.start.__copy__()
            symbol.term = (symbol.name in self.g.terminal)
            self.g.start = symbol
        for n in self.srcinfo:
            if not self.g.anchor_has(n):
                t = self.srcinfo[n]
                self.g.anchor_set(n, t[0], t[1])
        self.g.file_name = self.file_name
        return 0

    def _process_grammar (self, args):
        for arg in args:
            self._cache.append(arg)
            if arg.name == 'END':
                hr = self._process_rule(self._cache)
                self._cache.clear()
                if hr != 0:
                    return hr
        return 0

    def _process_rule (self, args):
        if not args:
            return 0
        argv = [n for n in args]
        if argv[0].name == 'STRING':
            self.error_token(argv[0], 'string literal %s can not have a rule'%argv[0].value)
            return 1
        elif argv[0].name != 'NAME':
            self.error_token(argv[0], 'wrong production head: "%s"'%argv[0].value)
            return 1
        elif argv[-1].name != 'END':
            self.error_token(argv[-1], 'missing ";"')
            return 2
        head = grammar.load_symbol(argv[0].value)
        argv = argv[:-1]
        if len(argv) < 2:
            self.error_token(argv[0], 'require ":" after "%s"'%(argv[0].value))
            return 3
        elif argv[1].name != ':':
            self.error_token(argv[1], 'require ":" before "%s"'%(argv[1].value))
            return 4
        cache = []
        for arg in argv[2:]:
            if arg.name == 'BAR':
                hr = self._add_rule(head, cache)
                cache.clear()
                if hr != 0:
                    return hr
            else:
                cache.append(arg)
        hr = self._add_rule(head, cache)
        if hr != 0:
            return hr
        if not self.g.anchor_has(head):
            self.g.anchor_set(head, self.file_name, argv[0].line)
        return 0

    def _add_rule (self, head, argv):
        body = []
        # print('add', head, ':', argv)
        pos = 0
        size = len(argv)
        action = {}
        precedence = None
        while pos < size:
            token = argv[pos]
            if token.name == 'STRING':
                text = token.value
                value = cstring.string_unquote(text)
                if not value:
                    pos += 1
                    continue
                elif len(text) < 2:
                    self.error_token(token, 'bad string format %s'%text)
                    return 10
                elif len(text) == 2:
                    pos += 1
                    continue
                symbol = grammar.load_symbol(token.value)
                body.append(symbol)
                pos += 1
                self.g.push_token(symbol)
            elif token.name == 'NAME':
                symbol = grammar.load_symbol(token.value)
                body.append(symbol)
                pos += 1
            elif token.name == 'MACRO':
                cmd = token.value.strip()
                pos += 1
                if cmd == '%prec':
                    token = argv[pos]
                    prec = token.value
                    pos += 1
                    if prec not in self.g.precedence:
                        self.error_token(token, 'undefined precedence %s'%prec)
                        return 11
                    precedence = prec
                elif cmd in ('%empty', '%e', '%epsilon'):
                    pos += 1
                    continue
            elif token.name == 'ACTION':
                i = len(body)
                if i not in action:
                    action[i] = []
                act = (token.value, i)
                action[i].append(act)
                pos += 1
            elif token.name == 'NUMBER':
                self.error_token(token)
                return 11
            elif token.name == 'OPERATOR':
                self.error_token(token)
                return 12
            elif token.name == 'MISMATCH':
                self.error_token(token)
                return 13
            else:
                self.error_token(token)
                return 14
            pass
        p = Production(head, body)
        p.precedence = precedence
        if len(action) > 0:
            p.action = action
        # print('action:', action)
        self.g.append(p)
        for token in argv:
            if token.value not in self.srcinfo:
                t = (self.file_name, token.line)
                self.srcinfo[token.value] = t
        if argv:
            self.g.anchor_set(p, self.file_name, argv[0].line)
        return 0

    def _process_macro (self, args):
        macro = args[0]
        argv = args[1:]
        cmd = macro.value
        if cmd == '%token':
            for n in argv:
                if n.name != 'NAME':
                    self.error_token(n)
                    return 1
                self.g.push_token(n.value)
        elif cmd in ('%left', '%right', '%nonassoc', '%precedence'):
            assoc = cmd[1:].strip()
            for n in argv:
                if n.name not in ('NAME', 'STRING'):
                    # print('fuck', n)
                    self.error_token(n)
                    return 1
                self.g.push_precedence(n.value, self.precedence, assoc)
            self.precedence += 1
        elif cmd == '%start':
            if len(argv) == 0:
                self.error_token(macro, 'expect start symbol')
                return 2
            token = argv[0]
            if token.name in ('STRING',):
                self.error_token(token, 'can not start from a terminal')
                return 3
            elif token.name != 'NAME':
                self.error_token(token, 'must start from a non-terminal symbol')
                return 4
            symbol = grammar.load_symbol(argv[0].value)
            if symbol.name in self.g.terminal:
                symbol.term = True
            if symbol.term:
                self.error_token(token, 'could not start from a terminal')
                return 5
            self.g.start = symbol
        return 0

    def _process_lexer (self, args):
        assert len(args) == 1
        args[0].column = -1
        origin: str = args[0].value.strip('\r\n\t ')
        m = re.match(r'[@]\s*(\w+)', origin)
        if m is None:
            self.error_token(args[0], 'bad lex declaration')
            return 1
        head: str = ('@' + m.group(1)).strip('\r\n\t ')
        body: str = origin[m.span()[1]:].strip('\r\n\t ')
        if head in ('@ignore', '@skip'):
            if not ctr.validate_pattern(body):
                self.error_token(args[0], 'bad regex pattern: ' + repr(body))
                return 2
            self.g.push_scanner(('ignore', body))
        elif head == '@match':
            m = re.match(r'(\{[^\{\}]*\}|\w+)\s+(.*)', body)
            if m is None:
                self.error_token(args[0], 'bad lex matcher definition')
                return 3
            name = m.group(1).strip('\r\n\t ')
            pattern = m.group(2).strip('\r\n\t ')
            if not ctr.validate_pattern(pattern):
                self.error_token(args[0], 'bad regex pattern: ' + repr(pattern))
                return 4
            # print('matched name=%r patterm=%r'%(name, pattern))
            self.g.push_scanner(('match', name, pattern))
            if not name.startswith('{'):
                self.g.push_token(name)
        elif head == '@import':
            part = re.split(r'\W+', body)
            part = list(filter(lambda n: (n.strip('\r\n\t ') != ''), part))
            if len(part) == 1:
                name = part[0].strip()
                if not name:
                    self.error_token(args[0], 'expecting import name')
                    return 5
                if name not in PATTERN:
                    self.error_token(args[0], 'invalid import name "%s"'%name)
                    return 6
                self.g.push_scanner(('import', name, name))
                if not name.startswith('{'):
                    self.g.push_token(name)
            elif len(part) == 3:
                name = part[0].strip()
                if not name:
                    self.error_token(args[0], 'expecting import name')
                    return 7
                asname = part[2].strip()
                if not asname:
                    self.error_token(args[0], 'expecting aliasing name')
                    return 8
                if part[1].strip() != 'as':
                    self.error_token(args[0], 'invalid import statement')
                    return 9
                if name not in PATTERN:
                    self.error_token(args[0], 'invalid import name "%s"'%name)
                    return 10
                self.g.push_scanner(('import', asname, name))
                if not asname.startswith('{'):
                    self.g.push_token(asname)
        else:
            self.error_token(args[0], 'bad lex command: %r'%head)
        return 0


#----------------------------------------------------------------------
# load from file
#----------------------------------------------------------------------
def load_from_file(filename) -> Grammar:
    loader = GrammarLoader()
    g = loader.load_from_file(filename)
    if g is None:
        print('loading grammar failed')
        sys.exit(1)
    return g


#----------------------------------------------------------------------
# load from string
#----------------------------------------------------------------------
def load_from_string(code) -> Grammar:
    loader = GrammarLoader()
    g = loader.load(code)
    if g is None:
        print('loading grammar failed')
        sys.exit(1)
    return g


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        line = '% token t1 t2 NAME'
        cmd, body = cstring.string_match(line, r'[%]\s*(\w+)', 1)
        print(cmd)
        print(body)
        return 0
    def test2():
        g = load_from_file('grammar/test_bnf.txt')
        g.argument()
        g.print(0, False, True)
        print(list(g.terminal))
        print('start', g.start)
        import pprint
        pprint.pprint(g._anchor)
    def test3():
        g = load_from_file('grammar/ansi_c.txt')
        print(len(g.symbol))
        print(len(g))
        return 0
    def test4():
        g = load_from_file('grammar/test_lex1.txt')
        print(len(g.symbol))
        print(len(g))
        for scanner in g.scanner:
            print(scanner)
        return 0
    def test5():
        print('step1')
        g = load_from_file('grammar/json.txt')
        print('step2')
        return 0
    test5()



