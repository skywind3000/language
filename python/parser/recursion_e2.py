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
# S : S ( S ) S | <empty>
# -> eliminate left recursion
# S : <empty> S2
# S2: ( S ) S S2 | <empty>
#----------------------------------------------------------------------
tr: TokenReader = None

def S():
    S2()
    return 0

def S2():
    if tr.check('('):
        tr.expect('(')
        S()
        tr.expect(')')
        S()
        S2()
    else:
        pass
    return 0

def parse(code):
    global tr
    tr = token_input(code)
    S()
    print('accepted')
    print('')
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
        parse('(()(()))')
        return 0
    test2()



