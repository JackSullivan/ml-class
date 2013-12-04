package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{SVMExperiment, MaxEntExperiment}
import cc.factorie._

/**
 * @author John Sullivan
 */
object IPRCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).take(10000).toList
    implicit val random = scala.util.Random
    val (train, test) = patents.shuffle.split(0.7)
    val res = new MaxEntExperiment(_.iprcLabel).runExperiment(train, test)

    println(res)
  }
}
