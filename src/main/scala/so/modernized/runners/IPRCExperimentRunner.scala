package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{NaiveBayesExperiment, SVMExperiment, MaxEntExperiment}
import cc.factorie.app.classify.backend.NaiveBayes

/**
 * @author John Sullivan
 */
object IPRCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).toList
    implicit val random = scala.util.Random
    val MaxEntResults = new MaxEntExperiment(_.iprcLabel).runExperiment(patents)
    //val SVMResults = new SVMExperiment(_.iprcLabel).runExperiment(patents)
    //val NBResults = new NaiveBayesExperiment(_.iprcLabel).runExperiment(patents)
    println(MaxEntResults.testAccuracy)
//    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new NaiveBayesExperiment(_.iprcLabel))
//      .map {_.runExperiment(patents)}
  }
}
