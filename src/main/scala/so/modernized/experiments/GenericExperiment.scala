package so.modernized.experiments

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.app.classify._
import scala.util.Random
import scala.io.Source

/**
 * @author John Sullivan
 */


object GenericLabelDomain extends CategoricalDomain[String]
class GenericFeatureDomain(featList:Iterable[Any]) extends DiscreteDomain(featList)
class FeatureVectorVariable(features:Iterable[Double], _domain:GenericFeatureDomain) extends VectorVariable(new DenseTensor1(features.toArray)) {
  def domain = _domain

  def newValues(fValues:Iterable[Double]):FeatureVectorVariable = new FeatureVectorVariable(fValues, _domain)
}
class GenericLabel(label:String, featureList:Iterable[Double], featureDomain:GenericFeatureDomain) extends LabeledCategoricalVariable[String](label) {
  def domain = GenericLabelDomain

  lazy val _features = new FeatureVectorVariable(featureList, featureDomain)
  def features = _features
}

class GenericExperiment(trainer:LinearVectorClassifierTrainer, methodName:String) {
  def runExperiment(labels:Iterable[GenericLabel], testSplit:Double = 0.7)(implicit random:Random):ExperimentResult = {
    val (trainVariables, testVariables) = labels.shuffle.split(0.7)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    trainVariables.head.features.domain.freeze()

    val classifier = trainer.train(new LinearVectorClassifier(GenericLabelDomain.dimensionSize, trainVariables.head.features.domain.dimensionSize, {l:GenericLabel => l.features}), trainVariables, {l:GenericLabel => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}

object GenericExperiment {

  def readCSV(filename:String):Iterable[GenericLabel] = {
    var fDomain:Option[GenericFeatureDomain] = None
    Source.fromFile(filename).getLines().map { line =>
      val cells = line.split(",")
      if(fDomain.isEmpty) {
        fDomain = Option(new GenericFeatureDomain(cells.tail))
      }
      new GenericLabel(cells.head, cells.tail.map{_.toDouble}, fDomain.get)
    }.toList
  }

  def main(args:Array[String]) {

    implicit val random = Random
    val max = new GenericExperiment(new NaiveBayesClassifierTrainer(), "NB")

    val labels = readCSV("compressed_data/PCA1000.csv")
    println(max.runExperiment(labels))
  }
}
