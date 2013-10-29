package so.modernized

import scala.xml.Elem
import cc.factorie.app.nlp.Document

/**
 * @author John Sullivan
 */
case class Patent(id:String, sections:Iterable[String], claims:Iterable[String], abs:String, desc:String) {
  def asDocument:Document = {
    val doc = new Document(desc)
    //DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    doc
  }
}

object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
    (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").map{_.text},
    (patentXML \ "claims" \ "claim").map{
      claimNode =>
        (claimNode \ "claim-text").text
    },
    (patentXML \ "abstract").text,
    (patentXML \ "description").text
  )
}

