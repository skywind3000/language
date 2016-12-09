
def queen1()
	def queen(r, depth)
		for x in 1..8
			r[depth] = x
			ok = true
			for y in (depth - 1).step(1, -1)
				if r[y] == x
					ok = false
					break
				elsif r[y] == x - (depth - y)
					ok = false
					break
				elsif r[y] == x + (depth - y)
					ok = false
					break
				end
			end
			if ok
				if depth == 8
					r[0] += 1
				else
					queen(r, depth + 1)
				end
			end
		end
		r[0]
	end
	queen([0] * 9, 1)
end

def queen2()
	def check(r, r1, r2)
		d, x, y = (r2 - r1).abs, r[r1], r[r2]
		((x != y) && (x != y + d) && (x != y - d)) || r1 == r2
	end
	def queen(r, row)
		(1..8).each { |col|
			r[row] = col
			x = (1..row).reject { |y| check r, row, y }
			if x.length == 0
				if row == 8
					r[0] += 1
				else
					queen(r, row + 1)
				end
			end
		}
		r[0]
	end
	queen([0] * 9, 1)
end

puts queen1
puts queen2




