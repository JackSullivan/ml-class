package so.modernized.experiments

import scala.io.Source
import cc.factorie.variable._
import cc.factorie._
import cc.factorie.app.classify.{BatchOptimizingLinearVectorClassifierTrainer, LinearVectorClassifier, LinearVectorClassifierTrainer}
import scala.util.Random

/**
 * User: cellier
 * Date: 11/23/13
 * Time: 4:31 PM
 */
object LabelDomain extends CategoricalDomain[Int]

class SparseLabel(val features:BinaryFeatureVectorVariable[Int], val label:Int) extends LabeledCategoricalVariable[Int](label){
  def domain = LabelDomain
}

class SparseExperiment(trainer:LinearVectorClassifierTrainer, methodName:String) {
  def runExperiment(labels:Iterable[SparseLabel], testSplit:Double = 0.7)(implicit random:Random):ExperimentResult = {
    val (trainVariables, testVariables) = labels.shuffle.split(0.7)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    trainVariables.head.features.domain.freeze()

    val classifier = trainer.train(new LinearVectorClassifier(LabelDomain.dimensionSize, trainVariables.head.features.domain.dimensionSize, {l:SparseLabel => l.features}), trainVariables, {l:SparseLabel => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}

object SparseExperiment {
  def loadData(filename:String):Iterable[SparseLabel] = {
    object IndexDomain extends CategoricalVectorDomain[Int]

    val features:Iterable[BinaryFeatureVectorVariable[Int]] = Source.fromFile(filename+".vec").getLines().map { line =>
      line.split("\\s+")
    }.toList.groupBy(_(0)).mapValues(_.map(_(1).toInt)).values.map { featureIndices =>
      new BinaryFeatureVectorVariable[Int](featureIndices) {
        def domain = IndexDomain
      }
    }
    Source.fromFile(filename+".label").getLines().zip(features.toIterator).map{case (label, feature) =>
      new SparseLabel(feature ,label.split("\\s+")(1).toInt)
    }.toList
  }
  def main(args:Array[String]){
    implicit val random = Random
    val out = Seq("MaxEnt","SVM","NaiveBayes").map{method =>
        val max = new SparseExperiment(new BatchOptimizingLinearVectorClassifierTrainer()(random),method)
        max.runExperiment(loadData("sparse_data/even"))
    }
    out.foreach(t=> println(t.trainAccuracy + "," + t.testAccuracy))

  }
}


//BinaryFeatureVectorVariable