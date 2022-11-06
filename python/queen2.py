from __future__ import print_function
import time
import pprint

def check(m, depth):
    if depth < 1:
        return True
    x = m[depth]
    for i in range(depth):
        dx = m[i] - x
        dy = depth - i
        if dx == 0:
            return False
        elif dx == dy or dx == -dy:
            return False
    return True

def search(size):
    m = [0] * size
    count = 0
    top = 0
    while 1:
        succeed = False
        if check(m, top):
            if top == size - 1:
                count += 1
            succeed = True
        if (not succeed) or (top == size - 1):
            while top >= 0:
                m[top] += 1
                if m[top] < size:
                    break
                top -= 1
            if top < 0:
                break
        else:
           top += 1
           m[top] = 0
    return count

def benchmark(times, func):
    import time
    t = time.time()
    for i in range(times):
        func()
    t = time.time() - t
    print('benchmark time: %.3f'%t)

print(search(8))

N = 10
benchmark(N, lambda: search(10))
benchmark(N, lambda: search(10))
benchmark(N, lambda: search(10))



