#! /usr/bin/env python
# -*- coding: utf-8 -*-
#======================================================================
#
# ctoken_pygments.py - 
#
# Created by skywind on 2022/12/27
# Last Modified: 2022/12/27 18:07:46
#
#======================================================================
import pygments
import pygments.token
import pygments.lexers

from pygments.token import Token

import ctoken
import ctoken_reader



#----------------------------------------------------------------------
# ignore token type
#----------------------------------------------------------------------
_ignore_types = [
            Token.Text.Whitespace,
            Token.Comment.Multiline,
            Token.Comment.Single,
            Token.Comment.count
        ]


#----------------------------------------------------------------------
# translate type
#----------------------------------------------------------------------
_translate_types = {
            # Token.
            Token.Keyword: ctoken.KEYWORD,

        }


#----------------------------------------------------------------------
# get pygments tokens
#----------------------------------------------------------------------
def _pygments_get_tokens(source, lang = 'cpp'):
    import pygments
    import pygments.lexers
    lexer = pygments.lexers.get_lexer_by_name(lang)
    if isinstance(source, str):
        code = source
    else:
        code = source.read()
    tokens = lexer.get_tokens(code)
    return tokens


#----------------------------------------------------------------------
# pygments_tokens -> ctoken list
#----------------------------------------------------------------------
def translate(pygments_tokens):
    tokens = []
    reader = ctoken_reader.TokenReader(pygments_tokens)
    while not reader.is_eof():
        if reader.current is None:
            break
        if reader.current[0] in Token.String:
            text = ''
            while reader.current[0] in Token.String:
                text += reader.current[1]
                reader.advance(1)
            token = ctoken.Token(ctoken.STRING, text, None, None)
            tokens.append(token)
        elif reader.current[0] in Token.Keyword:
            token = ctoken.Token(ctoken.KEYWORD, reader.current[1], None, None)
            tokens.append(token)
        elif reader.current[0]: 
            pass
    return tokens


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        tokens = _pygments_get_tokens(open('../test_tok.c'), 'cpp')
        for t in tokens:
            print(t)
        return 0
    test1()

