package so.modernized
import cc.factorie.app.topics
import scala.xml.Elem
import cc.factorie.variable.CategoricalSeqDomain

/**
 * @author John Sullivan
 */
case class Patent(id:String, section:String, claims:Iterable[String], desc:String){
  def asLDADocument(implicit domain:CategoricalSeqDomain[String]):topics.lda.Document = topics.lda.Document.fromString(domain,id,desc)
}


object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
    (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").text,
    (patentXML \ "claims" \ "claim").map{
      claimNode =>
        (claimNode \ "claim-text").text
    },
    (patentXML\"description").text
  )
}

