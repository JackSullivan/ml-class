package so.modernized.experiments

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.app.classify._
import scala.util.Random
import so.modernized.{PatentPipeline, Patent}
import java.io.File


/**
 * User: cellier
 * Date: 11/24/13
 * Time: 3:18 PM
 */

class NormalExperiment(trainer:LinearVectorClassifierTrainer, methodName:String) {
  def runExperiment(patents:Iterable[Patent], testSplit:Double = 0.7)(implicit random:Random):ExperimentResult = {
    val (trainVariables, testVariables) = patents.map(patent => patent.iprcLabel).shuffle.split(0.7)
    println(trainVariables.size)
    //(trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    trainVariables.head.features.domain.freeze()
    println(trainVariables.size)
    val classifier = trainer.train(new LinearVectorClassifier(LabelDomain.dimensionSize, trainVariables.head.features.domain.dimensionSize, {l:Patent.Label => l.features}), trainVariables, {l:Patent.Label => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}

object NormalExperiment {
  def main(args:Array[String]){
    implicit val random = Random
    val patents = PatentPipeline("data/").toList
    println(patents.size)
    val max = new NormalExperiment(new BatchOptimizingLinearVectorClassifierTrainer()(random),"Maxent")
    val naivebayes = new NormalExperiment(new NaiveBayesClassifierTrainer(),"NaiveBayes")
    val svm = new NormalExperiment(new SVMLinearVectorClassifierTrainer()(random),"Svm")

    val out = Seq(max,svm,naivebayes).map{method =>
      method.runExperiment(patents)
    }
    val pw = new java.io.PrintWriter(new File("/output/supervisedExperiments"))
    try {
      out.foreach(t=> pw.write(t.trainAccuracy + "," + t.testAccuracy))
    } finally pw.close()

  }
}