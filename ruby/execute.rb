require 'Open3'

def backtick
	return `dir`
end

def capture
	text, code = Open3.capture2e 'dir'
	return text
end

x = backtick
puts '-' * 72
puts x

puts '-' * 72
x = capture
puts '-' * 72
puts x


