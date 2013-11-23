package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{SVMExperiment, MaxEntExperiment}

/**
 * @author John Sullivan
 */
object IPRCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).toList
    implicit val random = scala.util.Random

    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel))
      .map {_.runExperiment(patents)}
  }
}
