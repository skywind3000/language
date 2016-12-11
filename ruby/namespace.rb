class M1
	def self.test
		puts "M1.test"
	end
end

class M2
	class << M2
		def test
			puts "M2.test"
		end
	end
end

module M3
	module MM
		def self.test
			puts "M3.MM.test"
		end
	end
end

module M4
	def self.test
		puts "M4.test"
	end

	def M4.test2
		puts "M4.test2"
	end
end


M1.test
M2.test

M3::MM::test
M3::MM.test

M4.test
M4::test
M4.test2
M4::test2


