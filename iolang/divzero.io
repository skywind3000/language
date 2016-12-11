(1 / 1) println
(1 / 0) println

origDiv := Number getSlot("/")

Number / := method(x, if (x == 0, 0, self origDiv(x)))

"---------------" println

(5 / 1) println
(30 / 2) println
(100 / 0) println

for(i, 0, 4, i println)

