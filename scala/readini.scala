import scala.collection.mutable.HashMap

class IniReader(filename:String) {
	var sections = new HashMap[String, HashMap[String, String]]

	val lines = scala.io.Source.fromFile(filename).getLines
	var current = "default"

	for (line <- lines) {
		val text = line.trim
		if (text.indexOf('[') == 0 && text.indexOf(']') == text.length - 1) {
			current = text.substring(1, text.length - 1)
		}
		else if (text.indexOf('=') >= 0) {
			val pos:Int = text.indexOf('=')
			val key:String = text.substring(0, pos).trim
			val dat:String = text.substring(pos + 1).trim
			if (!sections.contains(current)) {
				sections.put(current, new HashMap[String, String])
			}
			sections(current) += key -> dat
			//println("key: " + key + " val:" + dat)
		}
	}

	def get(section:String, name:String, default:String):String = {
		if (!sections.contains(section)) {
			return default
		}
		val item = sections(section)
		if (!item.contains(name)) {
			return default
		}
		return item(name)
	}

	def display {
		sections.foreach { sect =>
			println("[" + sect._1 + "]")
			sect._2.foreach { item =>
				println("    " + item._1 + "=" + item._2)
			}
		}
	}
}


val ini = new IniReader("../data/win.ini")

println(ini.get("Mail", "MAPIXVER", ""))

println

ini.display

