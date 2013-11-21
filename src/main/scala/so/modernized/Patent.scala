package so.modernized

import scala.xml.Elem
import cc.factorie.app.topics.lda
import cc.factorie.variable._
import cc.factorie.app.strings.alphaSegmenter
import java.io.{FileWriter, BufferedWriter}
import cc.factorie.app.classify.LinearVectorClassifier


/**
 * @author John Sullivan
 */
case class Patent(id:String,iprcSections:Iterable[String], uspcSection:String,claims:Iterable[String], abs:String, desc:String,title:String) {
  def asLDADocument(implicit domain:CategoricalSeqDomain[String]):lda.Document = lda.Document.fromString(domain, id, desc)

  lazy val iprcLabel = new Patent.Label(new Patent.Features(preparedDesc), iprcSections.head, Patent.IPRCLabelDomain)
  lazy val uspcLabel = new Patent.Label(new Patent.Features(preparedDesc), uspcSection, Patent.USPCLabelDomain)

  var unsupervisedLabel:Option[Patent.Label] = None

  private lazy val preparedDesc:Iterable[String] = alphaSegmenter(desc).toSeq
  private lazy val preparedClaims:Iterable[Iterable[String]] = claims.map(claim => alphaSegmenter(claim).toSeq)
  private lazy val preparedAbs:Iterable[String] = alphaSegmenter(abs).toSeq
  def asVectorString(docNumber:Int) = iprcLabel.features.value.activeElements.map { case (index, value) =>
    "%d %d %.3f".format(docNumber + 1, index + 1, value)
  }.mkString("\n")

}

object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
  (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").map{_.text},
  (patentXML \ "us-bibliographic-data-grant" \ "classification-national" \ "main-classification").text,

    (patentXML \ "claims" \ "claim").map{
    claimNode =>
      (claimNode \ "claim-text").text
  },
  (patentXML \ "abstract").text,
  (patentXML \ "description").text,
  (patentXML \ "invention-title").text
  )

  def classifier(label:Label):LinearVectorClassifier[Label, Features] = new LinearVectorClassifier[Label, Features](label.domain.dimensionSize, FeatureDomain.dimensionSize, _.features)

  object CPCLabelDomain extends CategoricalDomain[String] {
    this ++= Vector("A", "B", "C", "D", "E", "F", "G", "H", "N")
    freeze()
  }

  object IPRCLabelDomain extends CategoricalDomain[String] {
    this ++= Vector("A", "B", "C", "D", "E", "F", "G", "H", "Y")
    freeze()
  }

  object USPCLabelDomain extends CategoricalDomain[String] {
    this ++= Vector("1", "B", "C", "D", "E", "F", "G", "H", "N")
  }

  object UnsupervisedLabelDomain extends CategoricalDomain[String] {
    this ++= Vector("1", "2", "3", "4", "5", "6", "7", "0")
    freeze()
  }

  object FeatureDomain extends CategoricalVectorDomain[String]

  class Label(val features:Features, labelString:String, val domain: CategoricalDomain[String]) extends LabeledCategoricalVariable[String](labelString)

  class Features(features:Iterable[String]) extends BinaryFeatureVectorVariable[String] {
    def domain = Patent.FeatureDomain

  }

  def main(args:Array[String]) {
    val outFilename = "sample_out"
    val labelFilename = "sample_labels"

    val wrt = new BufferedWriter(new FileWriter(outFilename))
    val labelWrt = new BufferedWriter(new FileWriter(labelFilename))

    val patents = PatentPipeline("data/").toList
    patents.foreach(_.iprcLabel)
    Patent.FeatureDomain.freeze()
    patents.zipWithIndex.foreach{ case (patent, index) =>
      println(patent.asVectorString(index))
      wrt.write(patent.asVectorString(index))
      wrt.write("\n")
      labelWrt.write("%d %d".format(index + 1, patent.iprcLabel.intValue + 1))
      labelWrt.write("\n")
    }
    wrt.flush()
    wrt.close()
    labelWrt.flush()
    labelWrt.close()

  }
}

