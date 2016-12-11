module RequireLib
	def self.test2
		puts "this is test2 from require_lib.rb"
	end
end


def RequireLib.test
	puts "this is test from require_lib.rb"
end

