
class Car
	def initialize(number)
		@number = number
	end

	def inc
		@number += 1
	end
end

class Bike
	def initialize(number)
		self.num = number
	end
	def inc
		self.num += 1
	end
end

c1 = Car.new(10)
c2 = Car.new(50)

puts c1.inc
puts c1.inc
puts c2.inc
puts c1.inc
puts c1.inc
puts c2.inc

puts '-' * 72
b1 = Bike.new(20)
b2 = Bike.new(60)
puts b1.inc
puts b1.inc
puts b2.inc
puts b1.inc
