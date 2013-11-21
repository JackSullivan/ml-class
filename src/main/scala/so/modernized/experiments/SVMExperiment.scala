package so.modernized.experiments

import so.modernized.PatentPipeline
import cc.factorie.app.classify.SVMLinearVectorClassifierTrainer
import cc.factorie._

/**
 * User: cellier
 * Date: 11/14/13
 * Time: 7:26 PM
 */
object SVMExperiment extends ClassifierExperiment{

  val trainer = new SVMLinearVectorClassifierTrainer()(random)

  def main(args: Array[String]){
    SVMExperiment.runExperiment(PatentPipeline("data/").toList.map(_.iprcLabel))(random)
  }
}

