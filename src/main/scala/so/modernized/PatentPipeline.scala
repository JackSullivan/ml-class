package so.modernized

import scala.xml.Elem
import scala.collection.JavaConverters._
import java.io.FileNotFoundException
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

/**
 * @author John Sullivan
 */
object PatentPipeline {
  def fromDir(dir:String):Stream[Elem] = Paths.get(dir) match {
    case path if Files.isDirectory(path) => Files.newDirectoryStream(path).iterator().asScala.toStream
      .filter(_.toString.endsWith(".zip")) // todo this is bad
      .flatMap{filePath =>
      val zipFile = new ZipFile(filePath.toFile)
      zipFile.entries().asScala.toStream.flatMap{zipEntry =>
        PatentReader(zipFile.getInputStream(zipEntry)).toStream
      }
    }
    case nonPath => throw new FileNotFoundException("%s is not a valid directory path" format nonPath.getFileName)
  }

  def apply(dir:String):Stream[Patent] = fromDir(dir).flatMap(PatentFilters.apply).map{Patent.fromXML}
}
