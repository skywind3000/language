
def grep(word, filename)
	File.open(filename, 'r') do |fp|
		number = 0
		fp.each do |line|
			number += 1
			text = "#{filename}:#{number}: #{line}"
			puts text if line.match(word) != nil
		end
	end
end


grep('Video', '../data/win.ini')


