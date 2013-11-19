package so.modernized

import scala.xml.Elem

/**
 * @author John Sullivan
 */
object PatentFilters {
  val filters:Seq[Elem => Boolean] = Seq(
    x => (x \ "us-bibliographic-data-grant" \ "classifications-ipcr").nonEmpty,
    x => (x \ "abstract").nonEmpty,
    x => (x \ "description").nonEmpty
  )

  def apply(patentXML:Elem):Option[Elem] = if(filters.foldLeft(true){case (res, filter) => res && filter(patentXML)}) Option(patentXML) else None
}
