#! /usr/bin/env python
# -*- coding: utf-8 -*-
#======================================================================
#
# cstring.py - string enhancement
#
# Created by skywind on 2014/09/18
# Last Modified: 2023/01/08 03:55
#
# Features:
#
#   - write/read text to/from file with appropriate encoding
#   - write/read json to/from file with appropriate encoding
#   - write log to file with appropriate encoding
#   - safe string <-> integer/bool conversion
#   - quote/unquote string
#   - tokenize string with regex patterns
#
# Note:
# 
#    For more infomation, please read the README file.
#
#======================================================================
from __future__ import unicode_literals, print_function
import sys
import re


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['tokenize', 'string_unquote', 'string_quote', 'quoted_normalize',
           'string_is_quoted', 'string_to_int', 'string_to_bool',
           'string_to_float',
           'load_file_content', 'save_file_content', 'load_file_text',
           'save_file_text', 'json_save', 'json_load', 'tabulify',
           'mlog', 'getopt', 'hexdump', 'print_binary', 'string_match'
           ]


#----------------------------------------------------------------------
# python 2/3 compatible 
#----------------------------------------------------------------------
if sys.version_info[0] < 3:
    range = xrange   # noqa: F821
else:
    unicode = str
    long = int

UNIX = (sys.platform[:3] != 'win') and True or False


#----------------------------------------------------------------------
# safe convert to int
#----------------------------------------------------------------------
def string_to_int(text, round = 0):
    text = text.strip('\r\n\t ').lstrip('+')
    minus = False
    if text.startswith('-'):
        minus = True
        text = text.lstrip('-')
    if text.startswith('0x'):
        round = 16
    elif text.startswith('0b'):
        round = 2
    text = text.rstrip('uUlLbB')
    try:
        x = int(text, round)
    except:
        x = 0
    if minus:
        x = -x
    return x


#----------------------------------------------------------------------
# safe convert to boolean
#----------------------------------------------------------------------
def string_to_bool(text, defval = False):
    if text is None:
        return defval
    text = text.strip('\r\n\t ')
    if text == '':
        return defval
    if text.lower() in ('true', '1', 'yes', 't', 'enable'):
        return True
    if text.lower() in ('0', 'false', 'no', 'n', 'f', 'disable'):
        return False
    x = string_to_int(text)
    if text.isdigit() or x != 0:
        return (x != 0) and True or False
    return defval


#----------------------------------------------------------------------
# convert to float
#----------------------------------------------------------------------
def string_to_float(text):
    text = text.strip('\r\n\t ').rstrip('f')
    if text.startswith('.'):
        text = '0' + text
    try:
        x = float(text)
    except:
        x = 0.0
    return x


#----------------------------------------------------------------------
# eval quoted string
#----------------------------------------------------------------------
def string_unquote(text):
    text = text.strip("\r\n\t ")
    if len(text) < 2:
        return text.replace('"', '').replace("'", '')
    mark = text[0]
    if mark not in ('"', "'"):
        return text.replace('"', '').replace("'", '')
    if text[-1] != mark:
        return text.replace('"', '').replace("'", '')
    pos = 1
    output = []
    size = len(text)
    m = {'\\n': '\n', '\\t': '\t', '\\"': '"',
         "\\'": "\\'", '\\r': '\r', '\\\\': '\\',
         }
    while pos < size - 1:
        ch = text[pos]
        if ch == '\\':
            nc = text[pos:pos + 2]
            pos += 2
            if nc == '\\u':
                u = text[pos:pos + 4]
                pos += 4
                try:
                    x = int('0x' + u, 16)
                except:
                    x = ord('?')
                output.append(chr(x))
            elif nc == '\\x':
                u = text[pos:pos + 2]
                pos += 2
                try:
                    x = int('0x' + u, 16)
                except:
                    x = ord('?')
                output.append(chr(x))
            elif nc in m:
                output.append(m[nc])
            else:
                output.append(nc)
        else:
            output.append(ch)
            pos += 1
    return ''.join(output)


#----------------------------------------------------------------------
# escape string to quoted string
#----------------------------------------------------------------------
def string_quote(text, escape_unicode = False):
    output = []
    output.append("'")
    for ch in text:
        cc = ord(ch)
        if ch == "'":
            output.append("\\'")
        elif (cc >= 256 and escape_unicode):
            nc = hex(ord(ch))[2:]
            nc = nc.rjust(4, '0')
            output.append('\\u' + nc)
        elif (cc >= 128 and escape_unicode) or cc < 32:
            nc = hex(cc)[2:]
            nc = nc.rjust(2, '0')
            output.append('\\x' + nc)
        else:
            output.append(ch)
    output.append("'")
    return ''.join(output)


#----------------------------------------------------------------------
# normalize single/double quoted string to single quoted string
#----------------------------------------------------------------------
def quoted_normalize(text, double = False):
    text = text.strip('\r\n\t ')
    if len(text) == 0:
        return ''
    mark = text[0]
    if mark not in ('"', "'"):
        return None
    if len(text) < 2:
        return None
    if text[-1] != mark:
        return None
    size = len(text)
    pos = 1
    newmark = (not double) and "'" or '"'
    output = []
    output.append(newmark)
    while pos < size - 1:
        ch = text[pos]
        if mark == "'" and ch == '"':
            nc = newmark == "'" and '"' or '\\"'
            output.append(nc)
            pos += 1
        elif mark == '"' and ch == "'":
            nc = newmark == '"' and "'" or "\\'"
            output.append(nc)
            pos += 1
        elif ch == mark:
            nc = newmark == ch and ('\\' + ch) or ch
            output.append(nc)
            pos += 1
        elif ch == newmark:
            nc = '\\' + ch
            output.append(nc)
            pos += 1
        elif ch != '\\':
            output.append(ch)
            pos += 1
        else:
            nc = text[pos:pos + 2]
            pos += 2
            if newmark == '"' and nc == "\\'":
                nc = "'"
            elif newmark == "'" and nc == '\\"':
                nc = '"'
            elif nc == '\\':
                nc = '\\\\'
            output.append(nc)
    output.append(newmark)
    return ''.join(output)


#----------------------------------------------------------------------
# check if it is a quoted string
#----------------------------------------------------------------------
def string_is_quoted(text):
    if len(text) < 2:
        return False
    mark = text[0]
    if mark not in ('"', "'"):
        return False
    if text[-1] != mark:
        return False
    return True


#----------------------------------------------------------------------
# match string and return matched text and remain text
#----------------------------------------------------------------------
def string_match(source, pattern, group = 0):
    m = re.match(pattern, source)
    if m:
        matched = m.group(group)
        span = m.span()
        return matched, source[span[1]:]
    return None, source


#----------------------------------------------------------------------
# load binary
#----------------------------------------------------------------------
def load_file_content(filename, mode = 'r'):
    if hasattr(filename, 'read'):
        try: content = filename.read()
        except: pass
        return content
    try:
        fp = open(filename, mode)
        content = fp.read()
        fp.close()
    except:
        content = None
    return content


#----------------------------------------------------------------------
# save file content
#----------------------------------------------------------------------
def save_file_content(filename, content, mode = 'w'):
    try:
        fp = open(filename, mode)
        fp.write(content)
        fp.close()
    except:
        return False
    return True


#----------------------------------------------------------------------
# load file and guess encoding
#----------------------------------------------------------------------
def load_file_text(filename, encoding = None):
    content = load_file_content(filename, 'rb')
    if content is None:
        return None
    if content[:3] == b'\xef\xbb\xbf':
        text = content[3:].decode('utf-8')
    elif encoding is not None:
        text = content.decode(encoding, 'ignore')
    else:
        text = None
        guess = [sys.getdefaultencoding(), 'utf-8']
        if sys.stdout and sys.stdout.encoding:
            guess.append(sys.stdout.encoding)
        try:
            import locale
            guess.append(locale.getpreferredencoding())
        except:
            pass
        visit = {}
        for name in guess + ['gbk', 'ascii', 'latin1']:
            if name in visit:
                continue
            visit[name] = 1
            try:
                text = content.decode(name)
                break
            except:
                pass
        if text is None:
            text = content.decode('utf-8', 'ignore')
    return text


#----------------------------------------------------------------------
# save file text
#----------------------------------------------------------------------
def save_file_text(filename, content, encoding = None):
    import codecs
    if encoding is None:
        encoding = 'utf-8'
    if (not isinstance(content, unicode)) and isinstance(content, bytes):
        return save_file_content(filename, content)
    with codecs.open(filename, 'w', 
            encoding = encoding, 
            errors = 'ignore') as fp:
        fp.write(content)
    return True


#----------------------------------------------------------------------
# load object from json file
#----------------------------------------------------------------------
def json_load(filename, encoding = None):
    text = load_file_text(filename, encoding)
    import json
    try:
        return json.loads(text)
    except:
        pass
    return None


#----------------------------------------------------------------------
# save object to json file
#----------------------------------------------------------------------
def json_save(filename, obj):
    import json
    text = json.dumps(obj, indent = 4)
    save_file_text(filename, text)
    return 0


#----------------------------------------------------------------------
# tokenize
#----------------------------------------------------------------------
def tokenize(code, specs):
    patterns = []
    definition = {}
    if not specs:
        return None
    for index in range(len(specs)):
        name, pattern = specs[index]
        pn = 'PATTERN%d'%index
        definition[pn] = name
        patterns.append((pn, pattern))
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in patterns)
    line_starts = []
    pos = 0
    index = 0
    while 1:
        line_starts.append(pos)
        pos = code.find('\n', pos)
        if pos < 0:
            break
        pos += 1
    line_num = 0
    for mo in re.finditer(tok_regex, code):
        kind = mo.lastgroup
        value = mo.group()
        start = mo.start()
        while line_num < len(line_starts) - 1:
            if line_starts[line_num + 1] > start:
                break
            line_num += 1
        line_start = line_starts[line_num]
        name = definition[kind]
        if name is None:
            continue
        elif callable(name):
            obj = name(value)
            name, value = None, None
            if isinstance(obj, list) or isinstance(obj, tuple):
                if len(obj) > 0: 
                    name = obj[0]
                if len(obj) > 1:
                    value = obj[1]
            else:
                name = obj
        yield (name, value, line_num + 1, start - line_start + 1)
    return 0


#----------------------------------------------------------------------
# tabulify: style = 0, 1, 2
#----------------------------------------------------------------------
def tabulify (rows, style = 0):
    colsize = {}
    maxcol = 0
    output = []
    if not rows:
        return ''
    for row in rows:
        maxcol = max(len(row), maxcol)
        for col, text in enumerate(row):
            text = str(text)
            size = len(text)
            if col not in colsize:
                colsize[col] = size
            else:
                colsize[col] = max(size, colsize[col])
    if maxcol <= 0:
        return ''
    def gettext(row, col):
        csize = colsize[col]
        if row >= len(rows):
            return ' ' * (csize + 2)
        row = rows[row]
        if col >= len(row):
            return ' ' * (csize + 2)
        text = str(row[col])
        padding = 2 + csize - len(text)
        pad1 = 1
        pad2 = padding - pad1
        return (' ' * pad1) + text + (' ' * pad2)
    if style == 0:
        for y, row in enumerate(rows):
            line = ''.join([ gettext(y, x) for x in range(maxcol) ])
            output.append(line)
    elif style == 1:
        if rows:
            newrows = rows[:1]
            head = [ '-' * colsize[i] for i in range(maxcol) ]
            newrows.append(head)
            newrows.extend(rows[1:])
            rows = newrows
        for y, row in enumerate(rows):
            line = ''.join([ gettext(y, x) for x in range(maxcol) ])
            output.append(line)
    elif style == 2:
        sep = '+'.join([ '-' * (colsize[x] + 2) for x in range(maxcol) ])
        sep = '+' + sep + '+'
        for y, row in enumerate(rows):
            output.append(sep)
            line = '|'.join([ gettext(y, x) for x in range(maxcol) ])
            output.append('|' + line + '|')
        output.append(sep)
    return '\n'.join(output)


#----------------------------------------------------------------------
# write application level log
#----------------------------------------------------------------------
def mlog(*args):
    import sys, codecs, os, time
    now = time.strftime('%Y-%m-%d %H:%M:%S')
    part = [ str(n) for n in args ]
    text = u' '.join(part)
    mm = sys.modules[__name__]
    logfile = mm.__dict__.get('_mlog_file', None)
    encoding = mm.__dict__.get('_mlog_encoding', 'utf-8')
    stdout = mm.__dict__.get('_mlog_stdout', True)
    if logfile is None:
        name = os.path.abspath(sys.argv[0])
        name = os.path.splitext(name)[0] + '.log'
        logfile = codecs.open(name, 'a', encoding = encoding, errors = 'ignore')
        mm._mlog_file = logfile
    content = '[%s] %s'%(now, text)
    if logfile:
        logfile.write(content + '\r\n')
        logfile.flush()
    if stdout:
        sys.stdout.write(content + '\n')
    return 0


#----------------------------------------------------------------------
# getopt: returns (options, args)
#----------------------------------------------------------------------
def getopt(argv):
    args = []
    options = {}
    if argv is None:
        argv = sys.argv[1:]
    index = 0
    count = len(argv)
    while index < count:
        arg = argv[index]
        if arg != '':
            head = arg[:1]
            if head != '-':
                break
            if arg == '-':
                break
            name = arg.lstrip('-')
            key, _, val = name.partition('=')
            options[key.strip()] = val.strip()
        index += 1
    while index < count:
        args.append(argv[index])
        index += 1
    return options, args


#----------------------------------------------------------------------
# hexdump
#----------------------------------------------------------------------
def hexdump(data, char = False):
    content = ''
    charset = ''
    lines = []
    if isinstance(data, str):
        if sys.version_info[0] >= 3:
            data = data.encode('utf-8', 'ignore')
    elif sys.version_info[0] == 2:
        if isinstance(data, unicode):
            data = data.encode('utf-8', 'ignore')
    if not isinstance(data, bytes):
        raise ValueError('data must be bytes')
    for i, _ in enumerate(data):
        if sys.version_info[0] < 3:
            ascii = ord(data[i])
        else:
            ascii = data[i]
        if i % 16 == 0: content += '%08X  '%i
        content += '%02X'%ascii
        content += ((i & 15) == 7) and '-' or ' '
        if (ascii >= 0x20) and (ascii < 0x7f): charset += chr(ascii)
        else: charset += '.'
        if (i % 16 == 15): 
            lines.append(content + ' ' + charset)
            content, charset = '', ''
    if len(content) < 60: content += ' ' * (58 - len(content))
    lines.append(content + ' ' + charset)
    limit = char and 104 or 58
    return '\n'.join([ n[:limit] for n in lines ])


#----------------------------------------------------------------------
# print binary
#----------------------------------------------------------------------
def print_binary(data, char = True):
    print(hexdump(data, char))
    return True


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        s = r"'nihc \' asdfasdf'"
        v = '"hahaha \\" suck \' haha"'
        print(s)
        print(quoted_normalize(s))
        print(quoted_normalize(s, True))
        print(v)
        print(quoted_normalize(v))
        print(quoted_normalize(v, True))
        return 0
    def test2():
        print(string_quote('\u3211'))
        print(string_quote('\u3211', 1))
        print(string_quote('\u3211\x12', 1))
        n = 'hello\u3211\x12\x32\x41\x42\x43'
        print(n)
        print(string_quote(n))
        return 0
    def test3():
        return 0
    def test4():
        print_binary('hello, world')
        return 0
    test2()


