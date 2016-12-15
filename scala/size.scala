
import scala.io._
import scala.actors._
import Actor._

object PageLoader {
	def getPageSize(url : String) = Source.fromURL(url).mkString.length
}

val urls2 = List("http://www.baidu.com/",
	"http://www.163.com/",
	"http://www.sina.com.cn",
	"http://corp.netease.com")

val urls3 = List("http://www.google.com.hk/",
	"http://www.cnn.com/",
	"http://www.cnn.com/",
	"http://www.python.org/")

val urls = List("http://192.168.1.2",
	"http://192.168.1.2",
	"http://192.168.1.2",
	"http://192.168.1.2")

def timeMethod(method: () => Unit)  {
	val start = System.nanoTime
	method()
	val end = System.nanoTime
	println("Method took " + (end - start) / 1000000000.0 + " seconds")
}

def getPageSizeSequentially() {
	for (url <- urls) {
		println("Size for " + url + ": " + PageLoader.getPageSize(url))
	}
}

def getPageSizeConcurrently() {
	val caller = self
	for (url <- urls) {
		actor { caller ! (url, PageLoader.getPageSize(url)) }
	}

	for (i <- 1 to urls.size) {
		receive {
			case (url, size) =>
				println("Size for " + url + ": " + size)
		}
	}
}


println("Sequential run:")
timeMethod { getPageSizeSequentially }

println("Concurrent run:")
timeMethod { getPageSizeConcurrently }


