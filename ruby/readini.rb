
# ini reader without regular expression
class IniReader
	def initialize filename
		@section = {}
		parse filename
	end

	def parse filename
		sect = 'default'
		File.open(filename, 'r') do |fp|
			fp.each do |line|
				line = line.strip()
				if "#;".include? line[0] 
					next
				elsif line[0] == '[' 
					line = line[1...-1] if line[-1] == ']'
					sect = line
				elsif line.include? '='
					key, val = line.split('=', 2)
					@section[sect] = {} unless @section.has_key? sect
					@section[sect][key.strip()] = val
				end
			end
		end
		@section
	end

	def get section
		@section.fetch(section, {})
	end

	def read sect, item
		@section.fetch(sect, {})[item]
	end

	def info
		@section.each { |sect, items|
			puts "#{sect}:"
			for m in items.keys
				p = items[m]
				puts "    #{m}: #{p}"
			end
		}
	end
end


ini = IniReader.new '../data/win.ini'
ini.info

puts ini.get('Mail')
puts ini.read 'Mail', 'MAPI'



