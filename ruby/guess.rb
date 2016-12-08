x = rand(10)

while true
	puts "enter your guess"
	y = gets
	if y.to_i == x
		puts "yes you are right, secret number is #{x}"
		break
	else
		puts "you are wrong"
	end
end


