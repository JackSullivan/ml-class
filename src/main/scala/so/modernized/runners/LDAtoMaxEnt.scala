package so.modernized.runners

import so.modernized.{Patent, PatentPipeline}
import so.modernized.experiments.{WordDomain, EvaluateLDA, LDAExperiment, MaxEntExperiment}
import scala.util.Random
import cc.factorie._
import so.modernized.Patent.{UnsupervisedLabelDomain, Label}

/**
 * User: cellier
 * Date: 11/21/13
 * Time: 6:02 PM
 */
object LDAtoMaxEnt {
  def main(args:Array[String]){
    val inDir = args(0)
    val patents = PatentPipeline(inDir).toIterable
    println("Number of Patents Read in: " + patents.size)
    val (training,testing) = patents.split(.7)
    implicit val random = scala.util.Random
    val ldaEx = new LDAExperiment(training,8)(random)
    ldaEx.saveModel("LDAExperiment.model")
    println(ldaEx.lda.topicsWordsAndPhrasesSummary(20, 10))
    new EvaluateLDA(ldaEx.patents,ldaEx.numTopics)
    testing.foreach{_.iprcLabel}
    Patent.FeatureDomain.freeze()
    testing.foreach{
      testPatent =>
        val doc = testPatent.asLDADocument(WordDomain)
        ldaEx.lda.addDocument(doc,random)
        ldaEx.lda.inferDocumentTheta(doc)
        ldaEx.lda.maximizePhisAndThetas()
        testPatent.unsupervisedLabel = Some(new Label(testPatent.iprcLabel.features,ldaEx.getLDADocTopic(doc),UnsupervisedLabelDomain))
  }
    val results = new MaxEntExperiment(_.unsupervisedLabel.get).runExperiment(testing)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  //  val multiclassPatents = ldaEx.getMultiClassPatents()
   // println("MultiClass Patents Labeled with Multiple Classes: " + multiclassPatents.map(_.))
  }
}
