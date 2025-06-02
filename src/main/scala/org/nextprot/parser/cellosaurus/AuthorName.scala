package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map
import scala.xml._



// -----------------------------------------------
class AuthorName(val newFormat: String):
// -----------------------------------------------
  var oldFormat: String = null
  var lastname: String = ""
  var firstnames: String = ""
  var suffix: String = null
  var invalid: Boolean = false

  // Constructor logic
  private val nameParts = newFormat.split(", ")
  if nameParts.length != 2 then
    invalid = true
    println(s"ERROR: unexpected format for author name: $newFormat")
  else
    lastname = nameParts(0)
    val firstnameList = scala.collection.mutable.ListBuffer[String]()
    for elem <- nameParts(1).split(" ") do
      elem match
        case "Jr." | "Sr." | "II" | "III" | "IV" | "2nd" | "3rd" | "4th" =>
          suffix = elem
        case _ =>
          firstnameList += elem
    firstnames = firstnameList.mkString(" ")
    oldFormat = revertFormat(newFormat)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  def revertFormat(newFormat: String): String =
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    val Array(lastname, firstnames) = newFormat.split(", ")
    val parts = scala.collection.mutable.ListBuffer[String]()
    parts += (lastname + " ")
    for elem <- firstnames.split(" ") do
      var subelNo = 0
      for subelem <- elem.split("-") do
        subelNo += 1
        if subelNo > 1 then parts += "-"
        subelem match
          case "Jr." | "Sr." | "II" | "III" | "IV" | "2nd" | "3rd" | "4th" =>
            parts += (" " + subelem)
          case s if s.contains(".") =>
            parts += s
          case s =>
            parts += s.substring(0,1) + "."
    parts.mkString


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  override def toString: String =
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    s"Author( new=$newFormat, old=$oldFormat, last=$lastname, first=$firstnames, sfx=$suffix, invalid=$invalid )"


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// Note: attributes with a null value do NOT appear
// Note: attributes with an empty string value DO appear ! 
// See default values for attributes on top
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  def toXML = 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    
    <person name={oldFormat} lastname={lastname} firstnames={firstnames} suffix={suffix} />


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
object AuthorNameTester {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  def main(args: Array[String]): Unit = {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


  val fOld = scala.io.Source.fromFile("../cellosaurus-api/data_in/ra-old.txt")
  val fNew = scala.io.Source.fromFile("../cellosaurus-api/data_in/ra-new.txt")
  try
    val oldLines = fOld.getLines()
    val newLines = fNew.getLines()
    var lineNo = 0

    // Iterate through both files line by line
    while oldLines.hasNext && newLines.hasNext do
      val lineOld = oldLines.next()
      val lineNew = newLines.next()
      lineNo += 1
      val authOld = lineOld.trim
      val authNew = lineNew.trim
      val author = new AuthorName(authNew)
      val authRev = author.oldFormat
      if authOld != authRev then
        println(s"line $lineNo : $authNew => $authRev but expected $authOld")
        println(s"author $author")
  finally
    fOld.close()
    fNew.close()

    // val a1 = new AuthorName("Smith, S. John Jr.")
    // println(a1)
    // val a2 = new AuthorName("Doe, Jane K.A.")
    // println(a2)
    // val a3 = new AuthorName("InvalidName")
    // println(a3)
    // println("---")
    // println(a1.toXML)
    // println(a2.toXML)
    // println(a3.toXML)

    println("End")

  }

}