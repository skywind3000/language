
# implement a raw strip

# verbose version
def strip1(str)
	p1 = 0
	while p1 < str.length 
		if str[p1] != ' ' && str[p1] != "\t" && str[p1] != "\n"
			break
		else
			p1 += 1
		end
	end
	p2 = str.length - 1
	while p2 > p1
		if str[p2] != ' ' && str[p2] != "\t" && str[p2] != "\n"
			break
		else
			p2 -= 1
		end
	end
	return str[p1...p2 + 1]
end

# less verbose version
def strip2(str)
	pattern = " \t\r\n"
	for p1 in (0...str.length)
		if not pattern.include? str[p1]
			break
		end
	end
	for p2 in (str.length - 1).step(p1, -1)
		if not pattern.include? str[p2]
			break
		end
	end
	str[p1..p2]
end

# regular expression version
def strip3 str
	str = str.gsub(/^\s*/, '')
	str = str.gsub(/\s*$/, '')
end


s = " \t \nasdfasd adsf j jjjk j\t  \n  "
puts "<" + strip1(s) + ">"
puts "<" + strip2(s) + ">"
puts "<" + strip3(s) + ">"
puts s.match(/^\s*(\S*)\s*$/)[4]
puts "----" * 10



