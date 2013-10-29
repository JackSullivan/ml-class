package so.modernized.experiments

import so.modernized.{Patent, PatentPipeline}
import cc.factorie.variable.CategoricalSeqDomain
import cc.factorie.app.topics.lda.LDA
import cc.factorie.random
import scala.util.Random
import cc.factorie.directed.DirectedModel

/**
 * User: cellier
 * Date: 10/29/13
 * Time: 4:03 PM
 */
class LDAExperiment(patents:Iterator[Patent])(implicit val random:Random) {
  implicit object WordDomain extends CategoricalSeqDomain[String]
  implicit val model = DirectedModel()
  val lda = new LDA(WordDomain, 8)
  patents.foreach(patent => {
    println(patent.id)
    val doc = patent.asLDADocument
    lda.addDocument(doc,random)
    lda.inferDocumentTheta(doc)
  })
  println(lda.documents.size)
  lda.inferTopics()
}

object LDAExperiment{
  def main(args:Array[String]){
    new LDAExperiment(PatentPipeline("data/"))(random)
  }
}
