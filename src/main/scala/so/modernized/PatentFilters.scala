package so.modernized

import scala.xml.{NodeSeq, Elem}

/**
 * @author John Sullivan
 */
class Filter(path:(Elem => NodeSeq), pred:(NodeSeq => Boolean), name:String) {
  def apply(doc:Elem):Boolean = {
    val elem = path(doc)
    val res = pred(elem)
    if(!res) {
       println("%s failed %s".format(elem,name))
    }
    res
  }
}

object PatentFilters {
  val filters:Seq[Filter] = Seq(
    new Filter(x => (x \ "us-bibliographic-data-grant" \ "classifications-ipcr"), _.nonEmpty, "cpc_nonempty"),
    new Filter(x => (x \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section"), _.map{_.text}.forall(Set("A", "B", "C", "D", "E", "F", "G", "H", "Y").contains), "cpc_in_domain"),
    new Filter(x => (x \ "us-bibliographic-data-grant" \ "classification-national" \ "main-classification"), _.nonEmpty, "uspto_nonempty"),
    new Filter(x => (x \ "us-bibliographic-data-grant" \ "classification-national" \ "main-classification"), s => Set("1", "2", "3", "4", "5", "6", "7", "8", "9","D","0").contains(s.text.trim.head.toString), "uspto_in_domain"), //todo this is rejecting too many things to start with
    new Filter(x => (x \ "abstract"), _.nonEmpty, "abstract_nonempty"),
    new Filter(x => (x \ "description"), _.nonEmpty, "desc_nonempty")
  )

  def apply(patentXML:Elem):Option[Elem] = if(filters.foldLeft(true){case (res, filter) => res && filter(patentXML)}) Option(patentXML) else None
}
