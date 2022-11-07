f = lambda A, x, y: y < 0 or (not (A[y] in (A[x], A[x] + (x - y), A[x] - (x - y))))
g = lambda A, x, y: (not x) or (f(A, x, y) and ((y < 0) or g(A, x, y - 1)))
h = lambda A, x: sum([ g(A, x, x - 1) and 1 or 0 for A[x] in range(len(A)) ])
q = lambda A, x: h(A, x) if (x == 7) else sum([ q(A, x + 1) for A[x] in range(8) if g(A, x, x - 1) ])

print(q([ 0 for i in range(8) ], 0))
