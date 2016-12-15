
class Employee(val name:String) {
	def work() = println("I am working")
}

trait Dev {
	def code() = println("I am coding")
}

class Principal(override val name:String) extends Employee(name) with Dev {
	override def work() = println("Pincipal " + name + " is working")
}

val fred = new Principal("Fred")
fred.work
fred.code


