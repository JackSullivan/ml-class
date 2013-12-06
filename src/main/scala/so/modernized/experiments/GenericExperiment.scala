package so.modernized.experiments

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.app.classify._
import scala.util.Random
import scala.io.Source
import cc.factorie.la.Tensor1

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
  def runExperiment(trainVariables:Iterable[GenericLabel], testVariables:Iterable[GenericLabel])(implicit random:Random):ExperimentResult = {
    trainVariables.head.features.domain.freeze()
    (trainVariables ++ testVariables).foreach(_.setRandomly)

    val classifier = trainer.train(new LinearVectorClassifier(GenericLabelDomain.dimensionSize, trainVariables.head.features.domain.dimensionSize, {l:GenericLabel => l.features}), trainVariables, {l:GenericLabel => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}

object GenericExperiment {

  def readCSV(filename:String, labelFilename:String):Iterable[GenericLabel] = {
    var fDomain:Option[GenericFeatureDomain] = None
    Source.fromFile(filename).getLines().zip(Source.fromFile(labelFilename).getLines()).map { case(featureLine, labelLine) =>
      val Array(_, label) = labelLine.split("\\s+")
      val cells = featureLine.split(",")
      if(fDomain.isEmpty) {
        fDomain = Option(new GenericFeatureDomain(cells))
      }
      new GenericLabel(label, cells.map{_.toDouble}, fDomain.get)
    }.toList
  }

  def scaleFeatures(labels:Iterable[GenericLabel]) {
    val minVals = new DenseTensor1(labels.head.features.domain.size)
    val maxVals = new DenseTensor1(labels.head.features.domain.size)
    labels.foreach{ label =>
      label.features.value.foreachActiveElement{ case (index, value) =>
        if(value > maxVals(index)) maxVals(index) = value
        if(value < minVals(index)) minVals(index) = value
      }
    }
    labels.foreach{
      label =>
        val t = label.features.value
        t.foreachActiveElement{ case (index, value) =>
          t(index) = value + minVals(index)
          //t(index) = (value - minVals(index))/(maxVals(index) - minVals(index))
        }
    }
  }

  def main(args:Array[String]) {

    implicit val random = Random
    //val max = new GenericExperiment(new NaiveBayesClassifierTrainer(), "NB")

    //val (trainLabels, testLabels) = readCSV("compressed_data/%s%s_new.csv".format("PCA", "3")).splitAt(2975)
    //scaleFeatures(trainLabels ++ testLabels)
    //val classifier = new GenericExperiment(new SVMLinearVectorClassifierTrainer()(random), "SVM")
    //println(classifier.runExperiment(trainLabels, testLabels))
    val res:Iterable[ExperimentResult] = Seq("PCA", "Laplacian").flatMap{ method =>
      Seq("3", "10", "50", "100", "1000").map{ size =>
        val (trainLabels, testLabels) = readCSV("compressed_data/%s%s_new.csv".format(method, size), "five_hundred_cpc_all.label").splitAt(2975)
      //println(trainLabels.head.features.value)
      scaleFeatures(trainLabels ++ testLabels)
        //(trainLabels ++ testLabels).foreach{ label =>
          //label.features.value.twoNormalize()
        //}
      //println(trainLabels.head.features.value)
        val classifier = new GenericExperiment(new NaiveBayesClassifierTrainer(), "Max Ent %s:%s".format(method,size))
        classifier.runExperiment(trainLabels, testLabels)
      }
    }

    println("\tTrain\tTest")
    res.foreach{ result =>
      println("%s\t%.2f\t%.2f".format(result.method, result.trainAccuracy * 100, result.testAccuracy * 100))
    }
    /*
    val res:Iterable[ExperimentResult] = Seq("PCA", "Laplacian").flatMap{ method =>
      Seq("3", "10", "50", "100", "1000").flatMap{ size =>
        Seq(/*new NaiveBayesClassifierTrainer() -> "Naive Bayes",*/ new BatchOptimizingLinearVectorClassifierTrainer()(random) -> "Maximium Entropy", new SVMLinearVectorClassifierTrainer()(random) -> "SVM").map{
          case (trainer, name) =>
          val classifier = new GenericExperiment(trainer, name)
          val (trainLabels, testLabels) = readCSV("compressed_data/%s%s_new.csv".format(method, size)).splitAt(2975)
          classifier.runExperiment(trainLabels, testLabels)
        }
      }
    }
    res.foreach(println)
    */
  }
}
