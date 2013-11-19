package so.modernized.experiments

import so.modernized.{Patent, PatentPipeline}
import cc.factorie.variable.{DiscreteSeqVariable, DiscreteSeqDomain, DiscreteDomain, CategoricalSeqDomain}
import cc.factorie.app.topics.lda.{Document, LDA}
import cc.factorie.random
import scala.util.Random
import cc.factorie.directed.DirectedModel
import java.io._
import scala.util.control.Breaks._
import cc.factorie.app.bib.LDAUtils.WordSeqDomain
import scala.Predef._

/**
 * User: cellier
 * Date: 10/29/13
 * Time: 4:03 PM
 */

object WordDomain extends CategoricalSeqDomain[String]
class LDAExperiment(patents:Iterator[Patent], ldaModel:LDA)(implicit val random:Random) {
  def this(patents:Iterator[Patent])(implicit random:Random) = this(patents, {
    val lda = new LDA(WordDomain, 8)(DirectedModel(), random)
    patents.foreach(patent => {
      println(patent.id)
      val doc = patent.asLDADocument(WordDomain)
      lda.addDocument(doc,random)
      lda.inferDocumentTheta(doc)
    })
    println(lda.documents.size)
    lda.inferTopics()
    lda})(random)
  def this(patents:Iterator[Patent],filename: String) =this(patents, {
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

  def saveModel(fileName:String) = {
    val file = new File(fileName)
    val pw = new PrintWriter(file)
    pw.println("/alphas")
    pw.println(lda.alphas.value.mkString(" "))
    pw.println()
    lda.documents.foreach(_.writeNameWordsZs(pw))
    pw.close()
  }
  val lda = ldaModel
  lda.documents.map(doc => doc.thetaArray.zipWithIndex.maxBy(_._1)._2)




}

object LDAExperiment{
  def main(args:Array[String]){
    new LDAExperiment(PatentPipeline("data/"))(random)
  }
}
