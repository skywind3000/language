import sys
import ctoken
import ctoken_reader
import ctokenizer
from ctoken_reader import TokenReader
from ctoken_type import *


#----------------------------------------------------------------------
# get token stream 
#----------------------------------------------------------------------
def token_input (code):
    ct = ctokenizer.CTokenizer(code)
    return ctoken_reader.TokenReader(ct)


#----------------------------------------------------------------------
# E : E + T { print('+') }
# E : E - T { print('-') }
# E : T
# T : T * F { print('*') }
# T : T / F { print('/') }
# T : F
# ---> left recursion removal
# E : T E2
# E2 : + T {print('+')} E2 | - T {print('-')} E2 | e;
# T : F T2
# T2 : * F {print('*')} T2 | / F {print('/')} T2 | e;
# ---
# F: A | + F | - F {print('NEG ')}
# A: number {print(value)} | ( E )
#----------------------------------------------------------------------
tr: TokenReader = None
stack = []

# E : T E2
def E():
    T()
    E2()
    return 0

# E2 : + T {print('+')} E2 | - T {print('-')} E2 | e;
def E2():
    if tr.check('+'):
        tr.expect('+')
        T()
        stack.append('+')
        E2()
    elif tr.check('-'):
        tr.expect('-')
        T()
        stack.append('-')
        E2()
    else:
        pass
    return 0

# T : F T2
def T():
    F()
    T2()
    return 0

# T2 : * F {print('*')} T2 | / F {print('/')} T2 | e;
def T2():
    if tr.check('*'):
        tr.expect('*')
        F()
        stack.append('*')
        T2()
    elif tr.check('/'):
        tr.expect('/')
        F()
        stack.append('/')
        T2()
    else:
        pass
    return 0

# F: A | + F | - F {print('NEG ')}
def F():
    if tr.check('+'):
        tr.expect('+')
        F()
    elif tr.check('-'):
        tr.expect('-')
        F()
        stack.append('N')
    else:
        A()
    return 0

# A: number {print(value)} | ( E )
def A():
    if tr.check('('):
        tr.expect('(')
        E()
        tr.expect(')')
    elif tr.check(CTOKEN_NUMBER):
        stack.append(int(tr.value()))
        tr.expect(CTOKEN_NUMBER)
    return 0

# S: E ENDLINE {calc} | E EOF {calc}
def S():
    global stack
    E()
    if tr.check(CTOKEN_ENDLINE) or tr.check(CTOKEN_EOF):
        tr.expect(tr.name())
    sp = []
    for n in stack:
        if n in ('+', '-', '*', '/'):
            p2 = sp.pop()
            p1 = sp.pop()
            if n == '+': x = p1 + p2
            elif n == '-': x = p1 - p2
            elif n == '*': x = p1 * p2
            elif n == '/': x = p1 / p2
            else: x = None
            sp.append(x)
        elif n == 'N':
            p = sp.pop()
            sp.append(-p)
        elif isinstance(n, int):
            sp.append(n)
        elif isinstance(n, float):
            sp.append(n)
    stack.clear()
    print('  = ', sp[0])
    return 0

def parse(code):
    global tr, stack
    tr = token_input(code)
    S()
    return 0


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        ti = token_input('1+2/2+(2-1)')
        print(list(ti))
        return 0
    def test2():
        parse('1+2*(3+4)-10')
        parse('1+2+3+4')
        print('')
        parse('1+-(3+2)\n1+2\n3+4/2')
        return 0
    def test3():
        print('Input your expression to evaluate:')
        for line in sys.stdin:
            parse(line)
        return 0
    test3()


