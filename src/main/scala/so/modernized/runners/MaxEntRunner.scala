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
    val numPatents = args(1).toInt
    val numFeatures = args(2).toInt
    val patents = PatentPipeline(dataDir).toList.take(numPatents)
    implicit val random = scala.util.Random
    patents.foreach{_.iprcLabel}
    Patent.preparetfidf(patents)
    Patent.compressBags(patents,numFeatures)


    val (train, test) = patents.toList.split(0.7)

    val results = new MaxEntExperiment(_.iprcLabel).runExperiment(train,test)
    println("Method: " + results.method + " Train Accuracy: " + results.trainAccuracy + " Test Accuracy: " + results.testAccuracy)
  }
}
