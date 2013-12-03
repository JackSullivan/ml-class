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
  def methodName:String
  def runExperiment(train:Iterable[Patent], test:Iterable[Patent])(implicit random:Random):ExperimentResult = {
    val (trainVariables, testVariables) = train.map(toLabel) -> test.map(toLabel)
    //val (trainVariables, testVariables) = patents.map(toLabel).shuffle.split(testSplit)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    Patent.FeatureDomain.freeze()

    val classifier = trainer.train(Patent.classifier(trainVariables.head), trainVariables, {l:Patent.Label => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective

    //println ("Train accuracy = "+ objective.accuracy(trainVariables))
    //println ("Test  accuracy = "+ objective.accuracy(testVariables))
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}
case class ExperimentResult(method:String, trainAccuracy: Double, testAccuracy: Double)

class MaxEntExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  def methodName = "MaxEnt"
  val trainer = new BatchOptimizingLinearVectorClassifierTrainer()(random)
}

class NaiveBayesExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  def methodName = "Naive Bayes"
  val trainer = new NaiveBayesClassifierTrainer()
}

class SVMExperiment(val toLabel:(Patent => Patent.Label)) extends ClassifierExperiment {
  def methodName = "SVM"
  val trainer = new SVMLinearVectorClassifierTrainer()(random)
}
