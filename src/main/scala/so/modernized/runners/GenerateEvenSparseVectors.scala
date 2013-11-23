package so.modernized.runners

import so.modernized.{PatentPipeline, Patent, PatentRegularizer}

/**
 * @author John Sullivan
 */
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

    Patent.writeSparseVector(new PatentRegularizer(numberVecs,labelFun).apply(PatentPipeline(dataDir)), labelFun, outputPrefix)
  }
}
