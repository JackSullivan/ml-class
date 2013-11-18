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
case class Patent(id:String, sections:Iterable[String], claims:Iterable[String], abs:String, desc:String,title:String) {
  def asLDADocument(implicit domain:CategoricalSeqDomain[String]):lda.Document = lda.Document.fromString(domain, id, desc)

  lazy val label = new Patent.Label(new Patent.Features(desc), sections.head)

  def asVectorString(docNumber:Int) = label.features.value.activeElements.map { case (index, value) =>
    "%d %d %.3f".format(docNumber + 1, index + 1, value)
  }.mkString("\n")

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

  def classifier:LinearVectorClassifier[Label, Features] = new LinearVectorClassifier[Label, Features](LabelDomain.dimensionSize, FeatureDomain.dimensionSize, _.features)

  object LabelDomain extends CategoricalDomain[String] {
    this ++= Vector("A", "B", "C", "D", "E", "F", "G", "H", "N")
    freeze()
  }

  object FeatureDomain extends CategoricalVectorDomain[String]

  class Label(val features:Features, labelString:String) extends LabeledCategoricalVariable[String](labelString) {
    def domain = Patent.LabelDomain
  }

  class Features(descString:String) extends BinaryFeatureVectorVariable[String] {
    def domain = Patent.FeatureDomain
    alphaSegmenter(descString).foreach { token =>
      this += token
    }
  }

  def main(args:Array[String]) {
    val outFilename = "sample_out"
    val labelFilename = "sample_labels"

    val wrt = new BufferedWriter(new FileWriter(outFilename))
    val labelWrt = new BufferedWriter(new FileWriter(labelFilename))

    val patents = PatentPipeline("data/").toList
    patents.foreach(_.label)
    Patent.FeatureDomain.freeze()
    patents.zipWithIndex.foreach{ case (patent, index) =>
      println(patent.asVectorString(index))
      wrt.write(patent.asVectorString(index))
      wrt.write("\n")
      labelWrt.write("%d %d".format(index + 1, patent.label.intValue + 1))
      labelWrt.write("\n")
    }
    wrt.flush()
    wrt.close()
    labelWrt.flush()
    labelWrt.close()

  }
}

