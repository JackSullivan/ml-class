package so.modernized.experiments

import so.modernized.{Patent, PatentPipeline}
import cc.factorie.variable.{CategoricalSeqDomain}
import cc.factorie.app.topics.lda.{Document, LDA}
import cc.factorie.random
import scala.util.Random
import cc.factorie.directed.DirectedModel
import java.io._
import scala.util.control.Breaks._
import scala.Predef._
/**
 * User: cellier
 * Date: 10/29/13
 * Time: 4:03 PM
 */

//implicit object WordDomain extends CategoricalSeqDomain[String]
object WordDomain extends CategoricalSeqDomain[String]

import so.modernized.Patent.{UnsupervisedLabelDomain, Label}
class LDAExperiment(val patents:Iterable[Patent], val lda:LDA)(implicit val random:Random) {
  def this(patents:Iterable[Patent])(implicit random:Random)= this(patents, {
    val lda = new LDA(WordDomain, 8)(DirectedModel(), random)
    patents.foreach(patent => {
      val doc = patent.asLDADocument(WordDomain)
      lda.addDocument(doc,random)
      lda.inferDocumentTheta(doc)
    })

    println(lda.documents.size)
    lda.inferTopics()
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
      val doc = new Document(WordDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
      doc.zs = new lda.Zs(Nil)
      if (doc.length > 1) lda.addDocument(doc, random) // Skip documents that have only one word because inference can't handle them
    }}
    reader.close()
    lda.maximizePhisAndThetas()
    lda
  })(random)

  def saveModel(fileName:String) = {
    val file = new File(fileName)
    val pw = new PrintWriter(file)
    pw.println("/alphas")
    pw.println(lda.alphas.value.mkString(" "))
    pw.println()
    lda.documents.foreach(_.writeNameWordsZs(pw))
    pw.close()
  }

  val docLabels = lda.documents.map(doc => (doc.name,doc.thetaArray.zipWithIndex.maxBy(_._1)._2)).toMap
  patents.foreach(patent => patent.unsupervisedLabel = Some(new Label(patent.iprcLabel.features,docLabels(patent.id).toString,UnsupervisedLabelDomain)))
}

object LDAExperiment{
  def main(args:Array[String]){
    MaxEntExperiment.runExperiment(new LDAExperiment(PatentPipeline("data/").toList)(random).patents.map(_.unsupervisedLabel.get))(random)
  }
}
