package so.modernized.experiments

import so.modernized.PatentPipeline
import cc.factorie._
import cc.factorie.app.classify.{BatchOptimizingLinearVectorClassifierTrainer}

/**
 * User: cellier
 * Date: 10/31/13
 * Time: 12:28 PM
 */
object MaxEntExperiment extends ClassifierExperiment {

  val trainer = new BatchOptimizingLinearVectorClassifierTrainer()(random)

  def main(args: Array[String]){

    MaxEntExperiment.runExperiment(PatentPipeline("data/").toList.map{_.iprcLabel})(random)
  }
}
