
scope = 'global'

def foo():
    print('scope', scope)
    # nonlocal scope = 'local'
    # print('scope', scope)

foo()

