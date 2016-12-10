fp = File.new('../data/win.ini', 'r')
while line = fp.gets
	puts line
end

fp.close

