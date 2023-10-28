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
# S : + S S | - S S | num
# -> add action
# S : + {print('(')} S {print(')+(')} S {print(')')}
#   | - {print('(')} S {print(')-(')} S {print(')')}
#   | num {print(num)}
#----------------------------------------------------------------------
tr: TokenReader = None

def S():
    if tr.check('+'):
        tr.expect('+')
        print('(', end='')
        S()
        print(')+(', end='')
        S()
        print(')', end='')
    elif tr.check('-'):
        tr.expect('-')
        print('(', end='')
        S()
        print(')-(', end='')
        S()
        print(')', end='')
    elif tr.check(CTOKEN_NUMBER):
        print(tr.value(), end='')
        tr.expect(CTOKEN_NUMBER)
    return 0

def parse(code):
    global tr
    tr = token_input(code)
    S()
    print('\n')
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
        parse('+ +1 2 - + 5 4 3')
        return 0
    test2()


