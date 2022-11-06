from __future__ import print_function
import time
import pprint

def check(m:list, depth:int):
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

def search(m:list, depth:int):
    size = len(m) - 1
    if depth >= size:
        return 0
    for x in range(size):
        m[depth] = x
        if check(m, depth):
            if depth == size - 1:
                m[size] += 1
            else:
                search(m, depth + 1)
    return 0

def solve(size = 8):
    m = [0] * (size + 1)
    search(m, 0)
    return m[size]

def benchmark(times, func):
    import time
    t = time.time()
    for i in range(times):
        func()
    t = time.time() - t
    print('benchmark time: %.3f'%t)

print(solve(8))

N = 10
benchmark(N, lambda: solve(10))
benchmark(N, lambda: solve(10))
benchmark(N, lambda: solve(10))


