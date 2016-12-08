
i = 0
while i < 5
	puts "Hello #{i}"
	i += 1
end

for i in (0...5)
	puts "Hello #{i}"
end

for i in (0...5) do puts "FUCK #{i}"; end

puts "SUCK #{i}" while (i += 1) < 10

(0..9).each do 
	|i|
	puts "each #{i}"
end

i = 3
begin puts "haha #{i}" ; i += 1 end while (i < 7)

while i < 10 do puts "fuck #{i}" ; i += 1 end

(0...10).each {|i| puts i}

5.times { puts 'hahahaha' }

