package so.modernized.experiments

import so.modernized.Patent
import scala.util.Random
import cc.factorie._
import cc.factorie.app.classify.{SVMLinearVectorClassifierTrainer, NaiveBayesClassifierTrainer, BatchOptimizingLinearVectorClassifierTrainer, LinearVectorClassifierTrainer}
import cc.factorie.variable.HammingObjective

/**
 * @author John Sullivan
 */
trait ClassifierExperiment {
  def trainer:LinearVectorClassifierTrainer
  def toLabel:(Patent => Patent.Label)
  def runExperiment(patents:Iterable[Patent], testSplit:Double = 0.7)(implicit random:Random) {
    val (trainVariables, testVariables) = patents.map(toLabel).shuffle.split(testSplit)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    Patent.FeatureDomain.freeze()

    val classifier = trainer.train(Patent.classifier(trainVariables.head), trainVariables, {l:Patent.Label => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))
    val objective = HammingObjective
    println ("Train accuracy = "+ objective.accuracy(trainVariables))
    println ("Test  accuracy = "+ objective.accuracy(testVariables))
  }
}

class MaxEntExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  val trainer = new BatchOptimizingLinearVectorClassifierTrainer()(random)
}

class NaiveBayesExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  val trainer = new NaiveBayesClassifierTrainer()
}

class SVMExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  val trainer = new SVMLinearVectorClassifierTrainer()(random)
}
