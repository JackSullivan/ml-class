package so.modernized

import scala.collection.mutable

/**
 * @author John Sullivan
 * takes a stream(iterator) of patents and draws until a given number of each label class are drawn. Warns if it gets to the end without the numbers
 */
class PatentRegularizer(number:Int, labelFunc:(Patent => Patent.Label)) {

  def apply(patents:Iterator[Patent]):Iterator[Patent] = {
    val counts = new mutable.HashMap[String, Int].withDefaultValue(number)
    patents.flatMap { patent =>
      val label = patent.iprcSections.head
      //val label = labelFunc(patent).categoryValue
      counts(label) -= 1
      if(counts(label) >= 0 && label != "D") {
        patent.iprcLabel
        Some(patent)
      } else {
        None
      }
    }
  }
}

object PatentRegularizer {
  def main(args:Array[String]) {
    val patents = new PatentRegularizer(200, _.iprcLabel)(PatentPipeline("data/"))
  }
}
