package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{LDAExperiment, MaxEntExperiment}
import cc.factorie._

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
    val ldaEx = new LDAExperiment(patents)(random)
    val (train, test) = ldaEx.patents.shuffle.split(0.7)

    val results = new MaxEntExperiment(_.unsupervisedLabel.get).runExperiment(train,test)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  }
}
