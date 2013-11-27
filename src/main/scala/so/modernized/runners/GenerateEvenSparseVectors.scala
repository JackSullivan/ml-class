package so.modernized.runners

import scala.collection.mutable
import so.modernized.{PatentPipeline, Patent, PatentRegularizer}

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
    val numberVecs = args(3).toInt

    val patents = new TrainTestSplit(150, labelFun).apply(new PatentRegularizer(numberVecs, labelFun).apply(PatentPipeline(dataDir)))

    Patent.writeSparseVector(patents._2 ++ patents._1, labelFun, outputPrefix + "_all")
    //Patent.writeSparseVector(patents._2, labelFun, outputPrefix + "_test")
  }
}
