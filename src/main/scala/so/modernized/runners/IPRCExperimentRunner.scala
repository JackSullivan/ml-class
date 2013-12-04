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

    val patents = PatentPipeline(inDir).toList
    implicit val random = scala.util.Random
    val (train, test) = patents.shuffle.split(0.7)
    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel))
      .map {_.runExperiment(train, test)}
  }
}
