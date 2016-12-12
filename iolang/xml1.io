Builder := Object clone

Builder indent := 0

Builder forward := method(
		space := self indent
		for (i, 0, space - 1, write(" "))
		writeln("<", call message name, ">")
		self indent = (self indent) + 4
		call message arguments foreach(
			arg,
			content := self doMessage(arg);
			if (content type == "Sequence", 
				for (i, 0, (self indent) - 1, write(" "))
				writeln(content)))
		self indent = (self indent) - 4
		for (i, 0, space - 1, write(" "))
		writeln("</", call message name, ">"))

Builder ul(
		li("Io"),
		li("Lua"),
		lu(
			li("Test"),
			li("Haha")),
		li("JavaScript"))


