
key := (Random value * 1000) ceil

while(true,
		"Enter your guess: " print
		x := File standardInput readLine asNumber
		((x asString) .. " has been entered") println
		if (x == key,
			("Yes you are right, secret key is: " .. (key asString)) println
			break)
		if (x < key, "your input is less than secret key" println)
		if (x > key, "your input is great than secret key" println)
	 )


