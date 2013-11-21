package so.modernized.experiments

import so.modernized.Patent
import scala.util.Random
import cc.factorie._
import cc.factorie.app.classify.LinearVectorClassifierTrainer
import cc.factorie.variable.HammingObjective
import so.modernized.Patent.Label

/**
 * @author John Sullivan
 */
trait ClassifierExperiment {
  def trainer:LinearVectorClassifierTrainer
  def runExperiment(patentLabels:Iterable[Label], testSplit:Double = 0.7)(implicit random:Random) {
    val (trainVariables, testVariables) = patentLabels.shuffle.split(testSplit)
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
