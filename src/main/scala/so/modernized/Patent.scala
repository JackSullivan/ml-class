package so.modernized

import scala.xml.Elem
import cc.factorie.app.topics.lda
import cc.factorie.variable.CategoricalSeqDomain

/**
 * @author John Sullivan
 */
case class Patent(id:String, sections:Iterable[String], claims:Iterable[String], abs:String, desc:String,title:String) {
  def asLDADocument(implicit domain:CategoricalSeqDomain[String]):lda.Document = lda.Document.fromString(domain, id, desc)
}

object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
    (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").map{_.text},
    (patentXML \ "claims" \ "claim").map{
      claimNode =>
        (claimNode \ "claim-text").text
    },
    (patentXML \ "abstract").text,
    (patentXML \ "description").text,
    (patentXML \ "invention-title").text
  )
}

