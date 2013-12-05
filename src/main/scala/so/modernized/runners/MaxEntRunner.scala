package so.modernized.runners

import so.modernized.{Patent, PatentPipeline}
import so.modernized.experiments.MaxEntExperiment
import cc.factorie._
/**
 * Created with IntelliJ IDEA.
 * User: cellier
 * Date: 12/5/13
 * Time: 5:03 PM
*/

object MaxEntRunner {
  def main(args: Array[String]){
    val dataDir = args(0)
    val patents = PatentPipeline(dataDir).take(40000)
    implicit val random = scala.util.Random

    val (train, test) = patents.toList.split(0.7)

    val results = new MaxEntExperiment(_.iprcLabel).runExperiment(train,test)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  }
}
