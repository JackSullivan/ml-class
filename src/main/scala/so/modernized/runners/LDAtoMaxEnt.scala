package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{EvaluateLDA, LDAExperiment, MaxEntExperiment}

/**
 * User: cellier
 * Date: 11/21/13
 * Time: 6:02 PM
 */
object LDAtoMaxEnt {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).toList
    implicit val random = scala.util.Random
    val ldaEx = new LDAExperiment(patents,8)(random)
    ldaEx.saveModel("LDAExperiment.model")
    new EvaluateLDA(ldaEx.patents,ldaEx.numTopics)
    val results = new MaxEntExperiment(_.unsupervisedLabel.get).runExperiment(ldaEx.patents)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  //  val multiclassPatents = ldaEx.getMultiClassPatents()
   // println("MultiClass Patents Labeled with Multiple Classes: " + multiclassPatents.map(_.))
  }
}
