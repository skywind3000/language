require "Open3"

if Gem.win_platform?
	text, code = Open3.capture2e 'dir', "\\"
else
	text, code = Open3.capture2e 'ls', '-la', '/'
end


puts text
puts code


