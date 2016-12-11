require "net/http"

class Foo
	def self.bar
		puts "from Foo.bar"
	end
end

class Fuck
	class << Fuck
		def fuck
			puts "fucked here"
		end
	end
end


Foo::bar

puts $LOAD_PATH
$LOAD_PATH.unshift('./')
require "require_lib"

RequireLib.test
RequireLib.test2
Fuck.fuck

puts File.dirname(__FILE__)

