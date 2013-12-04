package so.modernized.runners

import so.modernized.PatentPipeline
import so.modernized.experiments.{ExperimentResult, MaxEntExperiment}
import cc.factorie._

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

    val patents = PatentPipeline(inDir).toIterable
    println("Number Patents Loaded: "+ patents.size)
    implicit val random = scala.util.Random
    patents.foreach{pat => pat.uspcLabel}
    val (train, test) = patents.shuffle.split(0.7)
    val MaxEntResults:ExperimentResult = new MaxEntExperiment(_.uspcLabel).runExperiment(train, test)
    //val SVMResults = new SVMExperiment(_.uspcLabel).runExperiment(patents)
    //val NBResults = new NaiveBayesExperiment(_.uLabel).runExperiment(patents)
    println(MaxEntResults.testAccuracy)
    //    Seq(new MaxEntExperiment(_.iprcLabel), new SVMExperiment(_.iprcLabel), new NaiveBayesExperiment(_.iprcLabel))
    //      .map {_.runExperiment(patents)}
  }
}
