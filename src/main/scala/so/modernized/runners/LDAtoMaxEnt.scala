package so.modernized.runners

import so.modernized.{PatentRegularizer, Patent, PatentPipeline}
import so.modernized.experiments.{WordDomain, EvaluateLDA, LDAExperiment, MaxEntExperiment}
import scala.util.Random
import cc.factorie._
import so.modernized.Patent.{UnsupervisedLabelDomain, Label}
import scala.Some

/**
 * User: cellier
 * Date: 11/21/13
 * Time: 6:02 PM
 */
object LDAtoMaxEnt {
  def performLDA(trainPatents: Iterable[Patent], testPatents: Iterable[Patent])(implicit random: Random){
    val ldaEx = new LDAExperiment(trainPatents,7)(random)
    println(ldaEx.lda.topicsWordsAndPhrasesSummary(1000, 10))
    new EvaluateLDA(ldaEx.patents,ldaEx.numTopics)
    testPatents.foreach{
      testPatent =>
        val doc = testPatent.asLDADocument(WordDomain)
        ldaEx.lda.addDocument(doc,random)
        ldaEx.lda.inferDocumentTheta(doc)
        ldaEx.lda.maximizePhisAndThetas()
        testPatent.unsupervisedLabel = Some(new Label(testPatent.iprcLabel.features,ldaEx.getLDADocTopic(doc),UnsupervisedLabelDomain))
    }
  }

  def main(args:Array[String]){
    val inDir = args(0)
    val regularizer = new PatentRegularizer(1000,_.iprcLabel)
    val patents = regularizer.apply(PatentPipeline(inDir)).toIterable

    println("Number of Patents Read in: " + patents.size)
    implicit val random = scala.util.Random
    val patentGroups = patents.groupBy(_.iprcLabel.categoryValue).map(group => group._2.split(.7))
    val (training,testing) = (patentGroups.flatMap(_._1),patentGroups.flatMap(_._2))
    //val (training,testing) = patents.split(.7)
    performLDA(training,testing)(random)
    Patent.FeatureDomain.freeze()
    val (train, test) = testing.shuffle.split(0.7)

    val results = new MaxEntExperiment(_.unsupervisedLabel.get).runExperiment(train,test)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  //  val multiclassPatents = ldaEx.getMultiClassPatents()
   // println("MultiClass Patents Labeled with Multiple Classes: " + multiclassPatents.map(_.))
  }
}
