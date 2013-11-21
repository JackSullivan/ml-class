package so.modernized

import scala.collection.mutable

/**
 * @author John Sullivan
 * takes a stream(iterator) of patents and draws until a given number of each label class are drawn. Warns if it gets to the end without the numbers
 */
class PatentRegularizer(number:Int, labelFunc:(Patent => Patent.Label)) {
  def apply(patents:Iterator[Patent]):Iterator[Patent] = {
    val dimensionSize = labelFunc(patents.next()).domain.dimensionSize
    val patentBoxes = new mutable.ArrayBuffer[mutable.ArrayBuffer[Patent]](dimensionSize)
    (0 until dimensionSize).foreach{ idex =>
      patentBoxes += new mutable.ArrayBuffer[Patent](100)
    }
    while (!(0 until dimensionSize).foldLeft(true){case (running, idex) => running && patentBoxes(idex).size == number} && patents.hasNext) {
      val patent = patents.next()
      val index = labelFunc(patent).intValue
      if(patentBoxes(index).size < number) {
        patentBoxes(index) += patent
      }
    }
    if(!(0 until dimensionSize).foldLeft(true){case (running, idex) => running && patentBoxes(idex).size == number}) {
      println("WARNING! we didn't find enough of every class")
    }
    patentBoxes.flatten.toIterator
  }
}

object PatentRegularizer {
  def main(args:Array[String]) {
    val patents = new PatentRegularizer(200, _.iprcLabel)(PatentPipeline("data/"))
  }
}
