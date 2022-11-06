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

function search(size)
	local m = {}
	for i = 0, size do
		m[i] = 0
	end
	local count = 0
	local top = 0
	while 1 do
		local succeed = false
		if check(m, top) then
			if top == size - 1 then
				count = count + 1
			end
			succeed = true
		end
		if (not succeed) or (top == size - 1) then
			while top >= 0 do
				m[top] = m[top] + 1
				if m[top] < size then
					break
				end
				top = top - 1
			end
			if top < 0 then
				break
			end
		else
			top = top + 1
			m[top] = 0
		end
	end
	return count
end

function benchmark(times, func)
	local t = os.clock()
	for i = 1, times do
		func()
	end
	t = os.clock() - t
	print(string.format("benchmark time: %.3f", t))
end

print(search(8))

N = 10
benchmark(N, function () search(10) end)
benchmark(N, function () search(10) end)
benchmark(N, function () search(10) end)

