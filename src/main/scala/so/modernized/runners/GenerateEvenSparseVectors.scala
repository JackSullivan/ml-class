package so.modernized.runners

import scala.collection.mutable
import so.modernized.{PatentPipeline, Patent, PatentRegularizer}
import so.modernized.experiments.{NaiveBayesExperiment, MaxEntExperiment, SVMExperiment}

/**
 * @author John Sullivan
 */
class TrainTestSplit(testSize:Int, labelFunc:(Patent => Patent.Label)) {
  def apply(patents:Iterator[Patent]):(Iterator[Patent], Iterator[Patent]) = {
    val counts = new mutable.HashMap[String,Int].withDefaultValue(testSize)
    patents.partition{ patent =>
      val label = labelFunc(patent).categoryValue
      counts(label) -= 1
      counts(label) >= 0
    }
  }
}

object GenerateEvenSparseVectors {
  def main(args:Array[String]) {
    val dataDir = args(0)
    val outputPrefix = args(1)
    val labelFun = args(2) match {
      case "cpc" => p:Patent => p.iprcLabel
      case "us" => p:Patent => p.uspcLabel
      case other => throw new Exception("Unsupported label: %s" format other)
    }
    implicit val random = scala.util.Random
    val numberVecs = args(3).toInt

    //val (testIter, trainIter) = new TrainTestSplit(150, labelFun).apply(new PatentRegularizer(numberVecs, labelFun).apply(PatentPipeline(dataDir)))

    val patents = PatentPipeline(dataDir).take(40000)
    //val svm = new SVMExperiment(labelFun)
    //val max = new MaxEntExperiment(labelFun)
    //val nb = new NaiveBayesExperiment(labelFun)

    //val (trainLabels, testLabels) = trainIter.toList -> testIter.toList

    //println(svm.runExperiment(trainLabels, testLabels))
    //println(max.runExperiment(trainLabels, testLabels))
    //println(nb.runExperiment(trainLabels, testLabels))


    Patent.writeSparseVector(patents, labelFun, outputPrefix, 100000)

    //Patent.writeSparseVector(patents._2 ++ patents._1, labelFun, outputPrefix + "_all")
    //Patent.writeSparseVector(patents._2, labelFun, outputPrefix + "_test")
  }
}
