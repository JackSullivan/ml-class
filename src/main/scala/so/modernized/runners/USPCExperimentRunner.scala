package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{ExperimentResult, NaiveBayesExperiment, SVMExperiment, MaxEntExperiment}

/**
 * User: cellier
 * Date: 12/1/13
 * Time: 8:19 PM
 */

object USPCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).toList
    implicit val random = scala.util.Random
    val (train, test) = patents.shuffle.split(0.7)

    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel))
      .map {_.runExperiment(train, test)}
  }
}
