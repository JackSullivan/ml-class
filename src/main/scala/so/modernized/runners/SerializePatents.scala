package so.modernized.runners

import so.modernized.PatentPipeline

/**
 * @author John Sullivan
 */
object SerializePatents {
  def main(args:Array[String]) {
    val inDir = args(0)
    val outFile = args(1)

    PatentPipeline(inDir)
  }
}
