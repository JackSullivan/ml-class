package so.modernized

import scala.xml.Elem
import scala.io.Source
import scala.xml.factory.XMLLoader
import javax.xml.parsers.{SAXParserFactory, SAXParser}
import cc.factorie._
import java.io.InputStream

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
  def apply(stream:InputStream):Iterator[Elem] = Source.fromInputStream(stream).getLines().toStream.split(_ != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").map{
    patentIter =>
      MyXML.loadString(patentIter.mkString("\n").replaceAll("""<\?[^\?]+\?>""", ""))
  }
}

