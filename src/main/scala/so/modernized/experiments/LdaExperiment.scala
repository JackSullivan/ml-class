package so.modernized.experiments

import so.modernized.{Patent, PatentPipeline}
import cc.factorie.variable.{DiscreteSeqVariable, DiscreteSeqDomain, DiscreteDomain, CategoricalSeqDomain}
import cc.factorie.app.topics.lda.{Doc, Document, LDA}
import cc.factorie.random
import scala.util.Random
import cc.factorie.directed.DirectedModel
import java.io._
import scala.util.control.Breaks._
import cc.factorie.app.bib.LDAUtils.WordSeqDomain
import scala.Predef._
import so.modernized.Patent.{UnsupervisedLabelDomain, Label}

/**
 * User: cellier
 * Date: 10/29/13
 * Time: 4:03 PM
 */

object WordDomain extends CategoricalSeqDomain[String]
class LDAExperiment(val patents:Iterable[Patent], val lda:LDA,val numTopics: Int = 8)(implicit val random:Random) {
  def this(patents:Iterable[Patent], numTopics: Int = 8)(implicit random:Random) = this(patents, {
    val lda = new LDA(WordDomain, numTopics)(DirectedModel(), random)

    patents.foreach(patent => {
      val doc = patent.asLDADocument(WordDomain)
      println(patent.id)
      lda.addDocument(doc,random)
      lda.inferDocumentTheta(doc)
    })

    println(lda.documents.size)
    lda.inferTopicsMultithreaded(3)
    lda})(random)
  def this(patents:Iterable[Patent],filename: String)(implicit random:Random) = this(patents, {
    val lda = new LDA(WordDomain, 8)(DirectedModel(),random)
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(new File(filename))))
    reader.mark(512)
    val alphasName = reader.readLine()
    if (alphasName == "/alphas") { // If they are present, read the alpha parameters.
      val alphasString = reader.readLine(); lda.alphas.value := alphasString.split(" ").map(_.toDouble) // set lda.alphas
      reader.readLine() // consume delimiting newline
    } else reader.reset() // Put the reader back to the read position when reader.mark was called
    breakable { while (true) {
      val doc = new Document(WordSeqDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
      doc.zs = new lda.Zs(Nil)
      lda.addDocument(doc, random) // Skip documents that have only one word because inference can't handle them
    }}
    reader.close()
    lda.maximizePhisAndThetas()
    lda
  })(random)
  def getLDADocTopic(doc: Doc): String = doc.thetaArray.zipWithIndex.maxBy(_._1)._2.toString

  def getMultiClassPatents():Iterable[Doc] ={
    lda.documents.filter{doc =>
      val maxScore = doc.thetaArray.sum
      doc.thetaArray.filter{_ / maxScore > .25}.length > 1
    }
  }
  def saveModel(fileName:String) = {
    val file = new File(fileName)
    val pw = new PrintWriter(file)
    pw.println("/alphas")
    pw.println(lda.alphas.value.mkString(" "))
    pw.println()
    lda.documents.foreach(_.writeNameWordsZs(pw))
    pw.close()
  }
  patents.foreach{_.iprcLabel}
  Patent.FeatureDomain.freeze()

  assert(lda.documents.zip(patents).forall{case (doc,pat) => doc.name == pat.id},"Zipped not Aligned")
//  val docLabels = lda.documents.map(doc => (doc.name,doc.thetaArray.zipWithIndex.maxBy(_._1)._2)).toMap
  patents.zip(lda.documents).par.foreach{case (patent,ldaDoc) => if(patent.id == ldaDoc.name) patent.unsupervisedLabel = Some(new Label(patent.iprcLabel.features,getLDADocTopic(ldaDoc),UnsupervisedLabelDomain)); else println("Patent LDA Doc does not line up")}
  println(patents.size)
}

object LDAExperiment{
  def main(args:Array[String]){
    val ldaEx = new LDAExperiment(PatentPipeline("data/").toList,"LDAModel.so")(random)
    //MaxEntExperiment.runExperiment(ldaEx.patents.map(_.unsupervisedLabel.get))(random)
  }
}
