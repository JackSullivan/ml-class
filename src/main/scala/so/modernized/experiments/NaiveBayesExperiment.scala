package so.modernized.experiments

import cc.factorie.app.classify.NaiveBayesClassifierTrainer
import so.modernized.PatentPipeline
import cc.factorie._

/**
 * @author John Sullivan
 */
object NaiveBayesExperiment extends ClassifierExperiment {
  val trainer = new NaiveBayesClassifierTrainer()

  def main(args:Array[String]) {
    NaiveBayesExperiment.runExperiment(PatentPipeline("data/").toList.map(_.iprcLabel))(random)
  }
}
