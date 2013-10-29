package so.modernized

import scala.xml.Elem
import scala.io.Source
import scala.xml.factory.XMLLoader
import javax.xml.parsers.{SAXParserFactory, SAXParser}

/**
 * @author John Sullivan
 */
object MyXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

object PatentReader {
  def readFile(filename:String):Iterator[Elem] = new SplitIterator[String](Source.fromFile(filename).getLines().toTraversable, _ != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").map{
  patentIter =>
    MyXML.loadString(patentIter.mkString("\n"))
  }

  def main(args:Array[String]) {
    val r = PatentReader.readFile("ipg120828.xml")
    val fR = r.filter(doc => (doc  \ "us-bibliographic-data-grant" \ "classifications-ipcr").nonEmpty)

    println((fR.next() \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").map{_.text})
  }
}

