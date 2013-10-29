package so.modernized

import scala.xml.Elem

/**
 * @author John Sullivan
 */
object PatentFilters {
  def apply(patentsXML:Iterator[Elem]):Iterator[Elem] = patentsXML.filter{patentXML => filters.foldLeft(true){
    case (res, filter) => res && filter(patentXML)
    }
  }

  val filters:Seq[Elem => Boolean] = Seq(x => (x  \ "us-bibliographic-data-grant" \ "classifications-ipcr").nonEmpty)
}
