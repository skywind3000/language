OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
		r := Map clone
		call message arguments foreach(arg, r doString(arg asString))
		r)

Map atPutNumber := method(
		self atPut(
			call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
			call evalArgAt(1)
			)
		)

Map asText := method(
			r := ""
			self asList foreach(item,
				key := item at(0)
				val := item at(1)
				x := (key asString) .. "=" .. (val serialized)
				r = r .. x .. " "
				)
			r
		)

Builder := Object clone do (

	indent := 0

	forward := method(
		write(" " repeated(self indent))
		text := ""
		call message arguments foreach(arg,
				if (arg name == "curlyBrackets",
					content := self doMessage(arg)
					data := (content asText) asMutable removeSuffix(" ")
					text = text .. " " .. data
				   )
			)

		writeln("<", call message name, text, ">")
		self indent = (self indent) + 4
		call message arguments foreach(
			arg,
			content := self doMessage(arg);
			if (content type == "Sequence", 
				for (i, 0, (self indent) - 1, write(" "))
				writeln(content))
			)
		self indent = (self indent) - 4
		write(" " repeated(self indent))
		writeln("</", call message name, ">"))
)

Builder ul(
		li("Io"),
		li("Lua"),
		lu(
			li("Test"),
			li("Haha")),
		fuck({ "Skywind" : "1234", "Bob": "5678"}),
		fuck({ "Skywind" : "1234", "Bob": "5678"}, {"dfddf":12, "ddd":23}, Suck()),
		haha(),
		li("JavaScript"))





