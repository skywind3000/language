
module ActsAsCsv
	def self.included(base)
		base.extend ClassMethods
	end
	module ClassMethods
		def acts_as_csv
			include InstanceMethods
		end
	end

	class CsvRow
		def initialize (headers, data)
			@headers = headers
			@data = data
		end
		def method_missing name, *args
			index = @headers.index(name.to_s)
			if index == nil
				nil
			else
				@data[index]
			end
		end
	end

	module InstanceMethods
		def read
			@csv_contents = []
			filename = self.class.to_s.downcase + '.txt'
			file = File.new(filename)
			@headers = file.gets.chomp.split(', ')
			file.each do |row|
				@csv_contents << row.chomp.split(', ')
			end
		end

		attr_accessor :headers, :csv_contents

		def initialize
			read
		end

		def each(&block)
			@csv_contents.each do |row|
				r = CsvRow.new(@headers, row)
				block.call r
			end
		end
	end
end

class RubyCsv
	include ActsAsCsv
	acts_as_csv
end

m = RubyCsv.new
puts m.headers.inspect
puts m.csv_contents.inspect
puts '-' * 72


m.each { |row| puts "#{row.name} -> #{row.size}" }


