package so.modernized

import java.nio.file.{Files, Paths}
import java.io.FileNotFoundException
import java.util.zip.ZipFile
import scala.collection.JavaConverters._
import scala.xml.factory.XMLLoader
import scala.xml.Elem
import javax.xml.parsers.{SAXParserFactory, SAXParser}
import scala.io.Source
import cc.factorie._

/**
 * @author John Sullivan
 */
object NonValidatingXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

object PatentPipeline {
  def apply(dir:String):Iterator[Patent] = Paths.get(dir) match {
    case path if Files.isDirectory(path) => {
      Files.newDirectoryStream(path).iterator().asScala
        .filter(_.toFile.getName.endsWith(".zip"))
        .flatMap {
        filePath =>
          val zipFile = new ZipFile(filePath.toFile)
          zipFile.entries().asScala
            .filter(_.getName.endsWith(".xml"))
            .flatMap {
            zipEntry =>
              Source.fromInputStream(zipFile.getInputStream(zipEntry)).getLines().toTraversable.split(_ != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                .flatMap {
                xmlString =>
                  val patentXML = NonValidatingXML.loadString(xmlString.mkString("\n"))
                  PatentFilters(patentXML).map(Patent.fromXML)
              }
          }
      }
    }
    case nonPath => throw new FileNotFoundException("%s is not a valid directory path" format nonPath)
  }

  def main(arg:Array[String]) {

  }
}
