#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# grammar.py - 
#
# Created by skywind on 2015/01/07
# Last Modified: 2019/01/07 20:21:06
#
#======================================================================
import sys
import json
import copy


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = ['GrammarError', 'Symbol', 'Vector', 'Production', 'Grammar']


#----------------------------------------------------------------------
# GrammarError
#----------------------------------------------------------------------
class GrammarError (Exception):
    pass


#----------------------------------------------------------------------
# 符号类：包括终结符和非终结符，term 代表是否为终结符，
# 空的话用空字符串表示
#----------------------------------------------------------------------
class Symbol (object):

    def __init__ (self, name, terminal = False):
        self.name = name
        self.term = terminal
    
    # 转为可打印字符串
    def __str__ (self):
        if not self.name:
            return "''"
        return self.name

    # 判断是否相等
    def __eq__ (self, symbol):
        if isinstance(symbol, str):
            return (self.name == symbol)
        elif symbol is None:
            return (self is None)
        elif not isinstance(symbol, Symbol):
            raise TypeError('Symbol cannot be compared to a %s'%type(symbol))
        return (self.name == symbol.name)

    def __ne__ (self, symbol):
        return (not (self == symbol))

    # >=
    def __ge__ (self, symbol):
        return (self.name >= symbol.name)

    # > 
    def __gt__ (self, symbol):
        return (self.name > symbol.name)

    # <=
    def __le__ (self, symbol):
        return (self.name <= symbol.name)

    # < 
    def __lt__ (self, symbol):
        return (self.name < symbol.name)

    def __repr__ (self):
        if not self.term:
            return '%s(%r)'%(type(self).__name__, self.name)
        return '%s(%r, %r)'%(type(self).__name__, self.name, self.term)

    # 求哈希，有这个函数可以将 Symbol 放到容器里当 key
    def __hash__ (self):
        return hash(self.name)

    # 拷贝
    def __copy__ (self):
        obj = Symbol(self.name, self.term)
        if hasattr(self, 'value'):
            obj.value = self.value
        if hasattr(self, 'token'):
            obj.token = self.token
        return obj

    # 深度拷贝
    def __deepcopy__ (self):
        obj = Symbol(self.name, self.term)
        if hasattr(self, 'value'):
            obj.value = copy.deepcopy(self.value)
        if hasattr(self, 'token'):
            obj.token = copy.deepcopy(self.token)
        return obj

    # 判断是否是字符串字面量
    def _is_literal (self):
        if len(self.name) < 2:
            return False
        mark = self.name[0]
        if mark not in ('"', "'"):
            return False
        if self.name[-1] == mark:
            return True
        return False

    # 判断是否是空/epsilon
    @property
    def is_epsilon (self):
        if self.term:
            return False
        elif self.name == '':
            return True
        if self.name in ('%empty', '%e', '%epsilon', '\u03b5', '<empty>'):
            return True
        return False

    # 判断是否是字符串字面量
    @property
    def is_literal (self):
        return self._is_literal()


#----------------------------------------------------------------------
# 从字符串或者 tuple 创建一个 Symbol
#----------------------------------------------------------------------
def load_symbol (source):
    if isinstance(source, Symbol):
        return source
    elif isinstance(source, str):
        sym = Symbol(source)
        if sym.is_literal:
            sym.term = True
            if len(sym.name) == 2 and sym.name[0] == sym.name[1]:
                sym.term = False
                sym.name = ''
            try: sym.value = eval(sym.name)
            except: pass
        elif source == '$':
            sym.term = True
        elif source == '#':
            sym.term = True
        return sym
    elif isinstance(source, list) or isinstance(source, tuple):
        assert len(source) > 0
        if len(source) == 0:
            raise ValueError('bad symbol: %r'%source)
        elif len(source) == 1:
            return Symbol(source[0])
        elif len(source) == 2:
            return Symbol(source[0], source[1])
        s = Symbol(source[0], source[1])
        s.value = source[2]
        return s
    raise ValueError('bad symbol: %r'%source)


#----------------------------------------------------------------------
# 符号矢量：符号列表
#----------------------------------------------------------------------
class Vector (object):

    def __init__ (self, vector):
        self.m = tuple(self.__load_vector(vector))
        self.__hash = None

    def __load_vector (self, vector):
        epsilon = True
        output = []
        p = [ load_symbol(n) for n in vector ]
        for symbol in p:
            if not symbol.is_epsilon:
                epsilon = False
                break
        if not epsilon:
            for n in p:
                if not n.is_epsilon:
                    output.append(n)
        return output

    def __len__ (self):
        return len(self.m)

    def __getitem__ (self, index):
        return self.m[index]

    def __contains__ (self, key):
        if isinstance(key, int):
            return (key >= 0 and key < len(self.m))
        for n in self.m:
            if n == key:
                return True
        return False

    def __hash__ (self):
        if self.__hash is None:
            h = tuple([n.name for n in self.m])
            self.__hash = hash(h)
        return self.__hash

    def __iter__ (self):
        return self.m.__iter__()

    def __repr__ (self):
        return '%s(%r)'%(type(self).__name__, self.m)

    def __str__ (self):
        body = [ str(n) for n in self.m ]
        return ' '.join(body)

    def __eq__ (self, p):
        assert isinstance(p, Vector)
        if hash(self) != hash(p):
            return False
        return (self.m == p.m)

    def __ne__ (self, p):
        return (not (self == p))

    def __ge__ (self, p):
        return (self.m >= p.m)

    def __gt__ (self, p):
        return (self.m > p.m)

    def __le__ (self, p):
        return (self.m <= p.m)

    def __lt__ (self, p):
        return (self.m < p.m)

    def __copy__ (self):
        obj = Vector(self.m)
        obj.__hash = self.__hash
        return obj

    def __deepcopy__ (self):
        p = [ n.__deepcopy__() for n in self.m ]
        obj = Vector(p)
        obj.__hash = self.__hash
        return obj

    def search (self, symbol, stop = -1):
        if stop < 0:
            return self.m.index(symbol)
        return self.m.index(symbol, stop)

    @property
    def is_empty (self):
        return (len(self.m) == 0)

    # 计算最左边的终结符
    def leftmost_terminal (self):
        for n in self.m:
            if n.term:
                return n
        return None

    # 计算最右边的终结符
    def rightmost_terminal (self):
        index = len(self.m) - 1
        while index >= 0:
            symbol = self.m[index]
            if symbol.term:
                return symbol
            index -= 1
        return None


#----------------------------------------------------------------------
# 产生式/生成式：由 head -> body 组成，head 是 symbol，
# budy 是一个 Vector，即 symbol 组成的序列
#----------------------------------------------------------------------
class Production (object):

    def __init__ (self, head, body, index = -1):
        self.head = load_symbol(head)
        self.body = Vector(body)
        self.__hash = None
        self.index = index
        self.is_epsilon = None
        self.has_epsilon = None
        self.precedence = None
        self.action = None

    def __len__ (self):
        return len(self.body)

    def __getitem__ (self, index):
        return self.body[index]

    def __contains__ (self, key):
        return (key in self.body)

    def __hash__ (self):
        if self.__hash is None:
            h1 = hash(self.head)
            h2 = hash(self.body)
            self.__hash = hash((h1, h2))
        return self.__hash

    def __iter__ (self):
        return self.body.__iter__()

    def __repr__ (self):
        return '%s(%r, %r)'%(type(self).__name__, self.head, self.body)

    def __str__ (self):
        body = [ str(n) for n in self.body ]
        return '%s: %s ;'%(self.head, ' '.join(body))

    def __eq__ (self, p):
        assert isinstance(p, Production)
        if hash(self) != hash(p):
            return False
        if self.head != p.head:
            return False
        return (self.body == p.body)

    def __ne__ (self, p):
        return not (self == p)

    def __ge__ (self, p):
        if self.head > p.head: 
            return True
        elif self.head < p.head:
            return False
        return (self.body >= p.body)

    def __gt__ (self, p):
        if self.head > p.head:
            return True
        elif self.head < p.head:
            return False
        return (self.body > p.body)

    def __lt__ (self, p):
        return (not (self >= p))

    def __le__ (self, p):
        return (not (self > p))

    def __copy__ (self):
        obj = Production(self.head, self.body)
        obj.index = self.index
        obj.precedence = self.precedence
        obj.is_epsilon = self.is_epsilon
        obj.has_epsilon = self.has_epsilon
        obj.__hash = self.__hash
        obj.action = self.action
        return obj

    def __deepcopy__ (self):
        p = self.body.__deepcopy__()
        obj = Production(self.head.__deepcopy__(), p)
        obj.index = self.index
        obj.precedence = self.precedence
        obj.is_epsilon = self.is_epsilon
        obj.has_epsilon = self.has_epsilon
        obj.__hash = self.__hash
        if self.action:
            obj.action = copy.deepcopy(self.action)
        return obj

    def search (self, symbol, stop = -1):
        return self.body.search(symbol, stop)

    @property
    def is_empty (self):
        return (len(self.body) == 0)

    # 计算最右边的终结符
    def rightmost_terminal (self):
        return self.body.rightmost_terminal()

    # 最左侧的终结符
    def leftmost_terminal (self):
        return self.body.leftmost_terminal()

    # 计算是否直接左递归
    @property
    def is_left_recursion (self):
        if len(self.body) == 0:
            return False
        return (self.head == self.body[0])

    # 计算是否直接右递归
    @property
    def is_right_recursion (self):
        if len(self.body) == 0:
            return False
        return (self.head == self.body[-1])

    def __action_to_string (self, m):
        if isinstance(m, str):
            return m
        assert isinstance(m, tuple)
        name:str = m[0]
        stack = m[1]
        if name.startswith('{') and name.endswith('}'):
            return '{%s/%d}'%(name[1:-1], stack)
        return '%s/%d'%(name, stack)

    # 返回包含动作的身体部分
    def stringify (self, head = True, body = True, action = False, prec = False):
        text = ''
        if head:
            text += str(self.head) + ': '
        act = getattr(self, 'action', {})
        if body:
            for i, n in enumerate(self.body.m):
                if action and act and (i in act):
                    for m in act[i]:
                        text += '%s '%self.__action_to_string(m)
                text += n.name + ' '
            i = len(self.body)
            if action and act and (i in act):
                for m in act[i]:
                    text += '%s '%self.__action_to_string(m)
        if prec:
            text += ' <%s>'%(self.precedence, )
        return text.strip('\r\n\t ')



#----------------------------------------------------------------------
# 语法类，一个语法 G 由终结符，非终结符和产生式组成
#----------------------------------------------------------------------
class Grammar (object):

    def __init__ (self):
        self.production = []
        self.symbol = {}            # str -> Symbol map
        self.terminal = {}          # str -> Symbol map
        self.rule = {}              # str -> list
        self.precedence = {}        # str -> prec
        self.assoc = {}             # str -> one of (None, 'left', 'right')
        self._anchor = {}           # str -> (filename, linenum)
        self._dirty = False
        self.scanner = []           # scanner rules
        self.start = None

    def reset (self):
        self.production.clear()
        self.symbol.clear()
        self.terminal.clear()
        self.nonterminal.clear()
        self.rule.clear()
        self._anchor.clear()
        self.scanner.clear()
        self.start = None
        return 0

    def _symbol_name (self, symbol):
        if isinstance(symbol, Symbol):
            return symbol.name
        elif isinstance(symbol, str):
            return symbol
        raise TypeError('bad symbol: %r'%symbol)

    def __len__ (self):
        return len(self.production)

    def __getitem__ (self, index):
        return self.production[index]

    def __iter__ (self):
        return self.production.__iter__()

    def __contains__ (self, key):
        if isinstance(key, int):
            return (key >= 0 and key < len(self.production))
        elif isinstance(key, Production):
            for p in self.production:
                if p == key:
                    return True
        elif isinstance(key, Symbol):
            return (key.name in self.symbol)
        elif isinstance(key, str):
            return (key in self.symbol)
        return False

    def __copy__ (self):
        obj = Grammar()
        for p in self.production:
            obj.push_production(p.__copy__())
        for t in self.terminal:
            obj.push_token(t)
        for p in self.precedence:
            c = self.precedence[p]
            obj.push_precedence(p, c[0], c[1])
        obj.srcinfo = self.srcinfo.__copy__()
        obj.update()
        if self.start:
            obj.start = obj.symbol[self.start.name]
        return obj

    def __deepcopy__ (self, memo):
        obj = Grammar()
        for p in self.production:
            obj.push_production(p.__deepcopy__(memo))
        for t in self.terminal:
            obj.push_token(t)
        for p in self.precedence:
            c = self.precedence[p]
            obj.push_precedence(p, c[0], c[1])
        obj.srcinfo = self.srcinfo.__deepcopy__(memo)
        obj.update()
        if self.start:
            obj.start = obj.symbol[self.start.name]
        return obj

    def insert (self, index, production):
        self.production.insert(index, production)
        self._dirty = True

    def search (self, p, stop = -1):
        if stop < 0:
            return self.production.index(p)
        return self.production.index(p, stop)

    def remove (self, index):
        if isinstance(index, int):
            self.production.pop(index)
        else:
            index = self.search(index)
            self.production.pop(index)
        self._dirty = True

    def pop (self, index = -1):
        self.production.pop(index)
        self._dirty = True

    def append (self, production):
        index = len(self.production)
        self.production.append(production)
        production.index = index
        self._dirty = True

    def replace (self, index, source):
        if isinstance(source, Production):
            self.production[index] = source
        elif isinstance(source, list) or isinstance(source, tuple):
            for n in source:
                assert isinstance(n, Production)
                self.production.insert(index + 1, n)
            self.production.pop(index)
        self._dirty = True

    def update (self):
        self.symbol.clear()
        self.rule.clear()
        for i, p in enumerate(self.production):
            p.index = i
            head = p.head
            if head.name not in self.symbol:
                self.symbol[head.name] = head
            for n in p.body:
                if n.name not in self.symbol:
                    self.symbol[n.name] = n
            if head.name not in self.rule:
                self.rule[head.name] = []
            self.rule[head.name].append(p)
        for n in self.terminal:
            s = self.terminal[n]
            if not s.term:
                s.term = True
        for n in self.symbol:
            s = self.symbol[n]
            s.term = (n in self.terminal)
            if not s.term:
                if s.name not in self.rule:
                    self.rule[s.name] = []
        for p in self.production:
            p.head.term = (p.head.name in self.terminal)
            for n in p.body:
                n.term = (n.name in self.terminal)
        for p in self.production:
            if p.precedence is None:
                rightmost = p.rightmost_terminal()
                if rightmost and (rightmost in self.precedence):
                    p.precedence = rightmost.name
        self._dirty = False
        return 0

    # declare terminal
    def push_token (self, token):
        name = self._symbol_name(token)
        if token not in self.terminal:
            t = load_symbol(token)
            t.term = True
            self.terminal[name] = t
        self._dirty = True
        return 0

    # push precedence
    def push_precedence (self, symbol, prec, assoc):
        name = self._symbol_name(symbol)
        if prec == 'precedence':
            prec = 'left'
        self.precedence[name] = prec
        self.assoc[name] = assoc

    # push scanner (aka. lexer) rules
    def push_scanner (self, obj):
        self.scanner.append(obj)
        return 0

    # create new symbol according to self.terminal
    def create_symbol (self, name):
        cc = load_symbol(name)
        if name != '':
            if name in self.terminal:
                cc.term = True
        return cc

    # argument
    def argument (self):
        if not self.start:
            raise GrammarError('no start point')
        if 'S^' in self.symbol:
            raise GrammarError('already argumented')
        head = 'S^'
        p = Production(head, [self.start])
        self.insert(0, p)
        self.start = p.head
        self.update()
        return 0

    def __anchor_key (self, obj):
        if isinstance(obj, str):
            return obj
        elif isinstance(obj, Symbol):
            return str(obj)
        elif isinstance(obj, int):
            return '^INT/' + str(obj)
        elif isinstance(obj, Vector):
            return '^VEC/' + str(obj)
        elif isinstance(obj, Production):
            return '^PROD/' + str(obj)
        return str(obj)

    # anchor: source file info (filename, line_num)
    def anchor_set (self, obj, filename, line_num):
        self._anchor[self.__anchor_key(obj)] = (filename, line_num)
        return 0

    def anchor_get (self, obj):
        key = self.__anchor_key(obj)
        if key not in self._anchor:
            return (None, None)
        return self._anchor[key]

    def anchor_has (self, obj):
        key = self.__anchor_key(obj)
        return (key in self._anchor)

    def print (self, mode = 0, action = False, prec = False):
        if mode == 0:
            for i, n in enumerate(self.production):
                t = '(%d) %s'%(i, n.stringify(True, True, action, prec))
                print(t)
        else:
            keys = list(self.rule.keys())
            for key in keys:
                head = str(key) + ': '
                padding = ' ' * (len(head) - 2)
                for i, p in enumerate(self.rule[key]):
                    if i == 0:
                        print(head, end = '')
                    else:
                        print(padding + '| ', end = '')
                    print(p.stringify(False, True, action, prec), end = ' ')
                    if len(self.rule[key]) > 1:
                        print('')
                if len(self.rule[key]) > 1:
                    print(padding + ';')
                else:
                    print(';')
        return 0

    def __str__ (self):
        text = []
        for i, n in enumerate(self.production):
            t = '(%d) %s'%(i, str(n))
            text.append(t)
        return '\n'.join(text)


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        s1 = Symbol('')
        s2 = Symbol('abc', True)
        print(s1, s2)
        print(repr(s1), repr(s2))
        return 0
    def test2():
        p = Production('E', ['T', "'*'", 'SUCK'])
        print(p)
        d = {}
        d['E'] = 5
        print(Symbol('E') in d)
        print(d[Symbol('E')])
        print(p.search(Symbol('SUCK')))
    def test3():
        g = Grammar()
        g.append(Production('E', ['T', "'*'", 'SUCK']))
        g.append(Production('E', ['T', "'/'", 'HAHA']))
        g.update()
        print(g)
        print(g.production[0][1:])
        print('cmp', g[0] >= g[1], g[0] > g[1], g[0] <= g[1], g[0] < g[1])
        return 0
    def test4():
        s1 = Symbol('ss', True)
        s2 = Symbol('ss', True)
        print(s1 == s2, s1 >= s2, s1 > s2, s1 <= s2, s1 < s2)
        s1 = Symbol('ss1', True)
        s2 = Symbol('ss2', True)
        print(s1 == s2, s1 >= s2, s1 > s2, s1 <= s2, s1 < s2)
        s1 = Symbol('ss', True)
        s2 = Symbol('ss', False)
        print(s1 == s2, s1 >= s2, s1 > s2, s1 <= s2, s1 < s2)
        return 0
    test3()


