package so.modernized

import scala.xml.Elem
import cc.factorie.app.topics.lda
import cc.factorie.variable._
import cc.factorie.app.strings.alphaSegmenter
import java.io.{FileWriter, BufferedWriter}
import cc.factorie.app.classify.LinearVectorClassifier
import scala.collection.mutable
import cc.factorie.la.Tensor1
import cc.factorie.app.nlp.lexicon.StopWords

/*
import scala.pickling._
import json._
*/


/**
 * @author John Sullivan
 */
case class Patent(id:String,iprcSections:Iterable[String], uspcSection:String,claims:Iterable[String], abs:String, desc:String,title:String) {
  def asLDADocument(implicit domain:CategoricalSeqDomain[String]):lda.Document = lda.Document.fromString(domain, id, desc)

  lazy val iprcLabel = new Patent.Label(new Patent.Features(preparedDesc), iprcSections.head, Patent.IPRCLabelDomain)
  lazy val uspcLabel = new Patent.Label(new Patent.Features(preparedDesc), uspcSection, Patent.USPCLabelDomain)

  var unsupervisedLabel:Option[Patent.Label] = None

  private lazy val preparedDesc:Iterable[String] = alphaSegmenter(desc).filterNot(StopWords.contains).toSeq
  private lazy val preparedClaims:Iterable[Iterable[String]] = claims.map(claim => alphaSegmenter(claim).filterNot(StopWords.contains).toSeq)
  private lazy val preparedAbs:Iterable[String] = alphaSegmenter(abs).filterNot(StopWords.contains).toSeq
  def asVectorString(docNumber:Int) = iprcLabel.features.value.activeElements.map { case (index, value) =>
    "%d %d %.3f".format(docNumber + 1, index + 1, value)
  }.mkString("\n")

}

object Patent {
  def fromXML(patentXML:Elem):Patent = Patent((patentXML \ "us-bibliographic-data-grant" \ "publication-reference" \ "document-id" \ "doc-number").text,
  (patentXML \ "us-bibliographic-data-grant" \ "classifications-ipcr" \ "classification-ipcr" \ "section").map{_.text},
  (patentXML \ "us-bibliographic-data-grant" \ "classification-national" \ "main-classification").text.head.toString,

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
    freeze()
  }

  object UnsupervisedLabelDomain extends CategoricalDomain[String] {
    this ++= Vector("1", "2", "3", "4", "5", "6", "7", "0")
    freeze()
  }

  object FeatureDomain extends CategoricalVectorDomain[String]

  class Label(val features:Features, labelString:String, val domain: CategoricalDomain[String]) extends LabeledCategoricalVariable[String](labelString)

  class Features(features:Iterable[String]) extends BinaryFeatureVectorVariable[String] {
    this ++= features
    def domain = Patent.FeatureDomain

  }

  def writeSparseVector(filename:String, readDir:String, number:Int, labelFunc:(Patent => Patent.Label) = _.iprcLabel, tfidf:Boolean = false) {
    val outFilename = "%s.vec" format filename
    val labelFilename = "%s.label" format filename
    val pipe = PatentPipeline(readDir)
    println("loaded")
    val patents:Iterable[Patent] = if(number != -1) {
      println("regularizing")
      val reg = new PatentRegularizer(number, labelFunc)
      reg(pipe).toStream
    } else {
      pipe.toStream
    }

    val wrt = new BufferedWriter(new FileWriter(outFilename))
    val labelWrt = new BufferedWriter(new FileWriter(labelFilename))
    //println("loaded patents")
    patents.foreach(_.iprcLabel)
    //println("initialized patents")
    if(tfidf){
      println("Preparing tfidf")
      Patent.preparetfidf(patents)
      println("compressing bags")
      Patent.compressBags(patents)
    }
    Patent.FeatureDomain.freeze()
    println("writing")
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

  //def serialize(patents:Iterable[Patent]) = patents.map(_.pickle)

  /*
  private def idfCounts(patents:ParIterable[Patent]):ParMap[String, Int] = patents.zipWithIndex.flatMap{ case(patent, index) =>
    patent.label.features.activeCategories.map{_ -> index}
  }.groupBy(_._1).mapValues(_.seq.map(_._2).toSet.size)

  @inline
  private def idf(term:String)(implicit idfCount:Map[String, Int], numDocs:Double):Double = math.log(numDocs / idfCount(term))

  def tfidf(patent:Patent, idfCount:Map[String, Int]):Map[String, Double] = {
    patent.label.features.activeCategories
  }
  */
  
  def preparetfidf(patents:Iterable[Patent]) {
    val patentVecs = patents map {_.iprcLabel.features.value}
    val idfs = idfCounts(patentVecs)
    println("generated idf counts")
    val counts = patentVecs.size
    patentVecs.foreach( patentVec => tfidf(patentVec, idfs, counts))
  }

  def idfCounts(docs:Iterable[Tensor1]):Map[Int, Double] = docs.flatMap{_.activeElements}.groupBy(_._1).seq.mapValues(_.view.map(_._2).sum)

  def tfidf(vec:Tensor1, idfs:Map[Int, Double], numDocs:Int) {
    if(vec.size > 0) {
      val maxWeight:Double = vec.max
      
      val newWords = new mutable.HashMap[Int, Double]()
      
      newWords ++= vec.activeElements.toSeq.map{case (word, count) =>
        word -> math.sqrt(count)/math.sqrt(maxWeight + 1) * math.log(numDocs/idfs.getOrElse(word, 1.0))
      }
      vec.zero()
      newWords.foreach{ case (index, value) =>
        vec(index) = value
      }
    }
  }

  def compressBags(ents:Iterable[Patent]) {
    ents.foreach{ ent =>
      trimBagTopK(ent.iprcLabel.features.value, 32)
    }
  }

  def trimBagTopK(vec:Tensor1, topK:Int) {
    if(vec.size > topK) {
      val topKItems = mutable.HashMap[Int, Double]()
      topKItems ++= vec.activeElements.toList.sortBy(_._2).reverse.take(topK)
      vec.zero()
      topKItems.foreach{ case (index, value) =>
        vec(index) = value
      }
    }
  }

  def main(args:Array[String]) {
    Patent.writeSparseVector("even", "data/", 200)
  }
  /*
  def main2(args:Array[String]) {
    val patents = PatentPipeline("data_less/").toStream.take(10)
    println(serialize(patents))
  }
  */
  def main1(args:Array[String]) {
    val outFilename = "sample_out"
    val labelFilename = "sample_labels"
    val tfidf = true

    val wrt = new BufferedWriter(new FileWriter(outFilename))
    val labelWrt = new BufferedWriter(new FileWriter(labelFilename))

    val patents = PatentPipeline("data/").toList
    patents.foreach(_.iprcLabel)
    Patent.FeatureDomain.freeze()
    println("writing")
    patents.zipWithIndex.foreach{ case (patent, index) =>
      //println(patent.asVectorString(index))
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

