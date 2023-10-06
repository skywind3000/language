local z = require('z')

local m = {[0] = 1, 2, 3, 4, [-2] = 3, linwei = 4, ['weilin'] = 5}

z.printT(m)

local m = {[0] = 100, 101, 102, 103}
print('size', #m)
z.printT(m)

for i, j in ipairs(m) do
	print(i, '->', j)
end

print('----')

for i, j in pairs(m) do
	print(i, '->', j)
end

