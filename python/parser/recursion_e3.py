import sys
import ctoken
import ctoken_reader
import ctokenizer
from ctoken_reader import TokenReader


#----------------------------------------------------------------------
# get token stream 
#----------------------------------------------------------------------
def token_input (code):
    ct = ctokenizer.CTokenizer(code)
    return ctoken_reader.TokenReader(ct)


#----------------------------------------------------------------------
# S : 0 S 1 | 0 1
# -> change
# S : 0 E
# E : S 1 | 1
#----------------------------------------------------------------------
tr: TokenReader = None

def S():
    tr.expect('0')
    E()
    return 0

def E():
    if tr.check('1'):
        tr.expect('1')
    else:
        S()
        tr.expect('1')
    return 0

def parse(code):
    global tr
    tr = token_input(code)
    S()
    print('accepted')
    print()
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
        parse('0 0 0 1 1 1')
        return 0
    test2()




