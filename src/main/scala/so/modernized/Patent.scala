package so.modernized

import scala.xml.Elem

/**
 * @author John Sullivan
 */
case class Patent(id:String, section:String, claims:Iterable[String])


object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
    (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").text,
    (patentXML \ "us-bibliographic-data-grant" \ "claims" \ "claims").map{
      claimNode =>
        (claimNode \ "claim-text").text
    }
  )
}

