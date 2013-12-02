package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{ExperimentResult, NaiveBayesExperiment, SVMExperiment, MaxEntExperiment}

/**
 * Created with IntelliJ IDEA.
 * User: cellier
 * Date: 12/1/13
 * Time: 8:19 PM
 * To change this template use File | Settings | File Templates.
 */
object USPCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).toList
    println("Number Patents Loaded: "+ patents.size)
    implicit val random = scala.util.Random
    val MaxEntResults:ExperimentResult = new MaxEntExperiment(_.uspcLabel).runExperiment(patents)
    //val SVMResults = new SVMExperiment(_.uspcLabel).runExperiment(patents)
    //val NBResults = new NaiveBayesExperiment(_.uLabel).runExperiment(patents)
    println(MaxEntResults.testAccuracy)
    //    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new NaiveBayesExperiment(_.iprcLabel))
    //      .map {_.runExperiment(patents)}
  }
}
