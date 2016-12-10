
a = (0...16).to_a

0.step(15, 4).each {|i| puts a[i,4].to_s }


a.each_slice(4) { |i| puts i.to_s }


