package org.nextprot.parser.cellosaurus

import scala.io.{Codec, Source}
import java.nio.charset.StandardCharsets
import java.nio.charset.CodingErrorAction

object Utf8Checker {

  def check(filename: String) : Boolean = {

    implicit val codec: Codec = Codec.UTF8
    codec.onMalformedInput(CodingErrorAction.REPLACE)

    var lineNo = 0
    var ok = true
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        lineNo += 1
        if (line.contains("�")) {
          ok = false
          println(s"ERROR at line ${lineNo}: ${line}")
        }
      }
    } catch {
      case e: Throwable => {
        ok = false
        println(s"Unexpected exception on checking NON-utf8 characters (at line ${lineNo}): ${e.getMessage()}")
      }
    }
    return ok
  }



  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val result = check(filename)
    println(s"ok: ${result}")
    println("End")
  }
}


