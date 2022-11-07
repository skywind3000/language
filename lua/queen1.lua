function check(m, depth)
	if depth < 1 then
		return true
	end
	local x = m[depth]
	for i = 0, (depth - 1) do
		local dx = m[i] - x
		local dy = depth - i
		if dx == 0 then 
			return false
		elseif dx == dy or dx == -dy then
			return false
		end
	end
	return true
end

function search(m, size, depth)
	if depth >= size then
		return 0
	end
	local count = 0
	for x = 0, (size - 1) do
		m[depth] = x
		if check(m, depth) then
			if depth == size - 1 then
				count = count + 1
			else
				count = count + search(m, size, depth + 1)
			end
		end
	end
	return count
end

function solve(size)
	local m = {}
	for i = 0, (size - 1) do
		m[i] = 0
	end
	return search(m, size, 0)
end

function benchmark(times, func)
	local t = os.clock()
	for i = 1, times do
		func()
	end
	t = os.clock() - t
	print(string.format("benchmark time: %.3f", t))
end

print(solve(8))

local N = 10
benchmark(N, function () solve(10) end)
benchmark(N, function () solve(10) end)
benchmark(N, function () solve(10) end)



