package so.modernized.runners

import so.modernized.{Patent, PatentPipeline}
import so.modernized.experiments.{NaiveBayesExperiment, SVMExperiment, MaxEntExperiment}
import cc.factorie._

/**
 * @author John Sullivan
 */
object IPRCExperimentRunner {
  def main(args:Array[String]) {
    val inDir = args(0)

    val patents = PatentPipeline(inDir).take(8000).toList
    val labelFun = {p:Patent => p.iprcLabel}
    implicit val random = scala.util.Random
    println("Found %d patents" format patents.size)
    patents.foreach(labelFun)
    println("Found %d features" format labelFun(patents.head).features.domain.dimensionSize)
    val reses = (0 until 1).map{runNumber =>
      val (train, test) = patents.shuffle.split(0.7)
      new MaxEntExperiment(labelFun).runExperiment(train, test)
    }
    reses.foreach(println)
    println("Average accuracy %.3f".format(reses.foldLeft(0.0)(_ + _.testAccuracy) / 5.0))
  }
}
