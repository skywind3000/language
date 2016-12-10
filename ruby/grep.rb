
def grep(word, filename)
	File.open(filename, 'r') do |fp|
		number = 0
		fp.each do |line|
			number += 1
			if line.match(word) != nil
				puts "#{filename}:#{number}: " + line
			end
		end
	end
end


grep('Video', '../data/win.ini')


