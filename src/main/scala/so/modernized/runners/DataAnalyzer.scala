package so.modernized.runners

import so.modernized.{Patent, PatentPipeline}
import cc.factorie._
import scala.collection.mutable
import cc.factorie.la.Tensor1
import cc.factorie.Tensor1

/**
 * User: cellier
 * Date: 12/5/13
 * Time: 5:50 PM
 */

object DataAnalyzer {
  def main(args:Array[String]){
    val dataDir = args(0)
    //val numWords:Int = args(1).toInt
    val patents = PatentPipeline(dataDir).take(40000)
    implicit val random = scala.util.Random
    val (train, test) = patents.toList.split(0.7)
    println(train.length)
    println(test.length)
    val groupedPatents = train.groupBy(_.iprcSections.head)
    groupedPatents.foreach{
            case (label,classPatents) =>
              label + ": " + classPatents.length
    }
    val tfidfVals = preparetfidf(train)
    tfidfVals.zipWithIndex.foreach{ case (tfidfVal, categoryIndex)=>
      print("topic" + categoryIndex+"\t")
      tfidfVal.foreachActiveElement{ case (index, value) =>
        print(Patent.FeatureDomain._dimensionDomain.dimensionName(index)+"\t")
      }
      println()
    }

//    groupedPatents.foreach{
//      case (label,classPatents) =>
//        label + ": " + classPatents.length
//        val classCounts = new mutable.HashMap[String,Int].withDefaultValue(0)
//        classPatents.foreach{pat=> pat.iprcLabel.features.activeCategories.foreach{word=> classCounts(word) += 1}}
//        val topWords = classCounts.sortReverse(_._2).take(numWords)
//        println(topWords.map{word => word._1 + ":" + word._2 +"\t"})
//    }
    //groupedPatents.foreach{case (label,classPatents)}
  }

  def preparetfidf(patents:Iterable[Patent]):Iterable[Tensor1] ={
    val otherVecs = patents.groupBy(_.iprcSections.head).map{_._2.map{_.iprcLabel.features.value}}
    val patentVecs = otherVecs.map{th => th.reduce(_+_)}
    println(patentVecs.size)
    val idfs = idfCounts(patentVecs)
    println("generated idf counts")
    val counts = patentVecs.size
    patentVecs.foreach{patentVec => tfidf(patentVec, idfs, counts); trimBagTopK(patentVec,32)}
    //patentVecs.foreach(trimBagTopK(_,10))
    patentVecs
  }
  def idfCounts(docs:Iterable[Tensor1]):Map[Int, Double] = docs.flatMap{_.activeElements}.groupBy(_._1).seq.mapValues(_.view.map(_._2).sum)

  def tfidf(vec:Tensor1, idfs:Map[Int, Double], numDocs:Int){
    if(vec.size > 0) {
      val maxWeight:Double = vec.max

      val newWords = new mutable.HashMap[Int, Double]()

      newWords ++= vec.activeElements.toSeq.map{case (word, count) =>
        word -> math.sqrt(count)/math.sqrt(maxWeight + 1) * math.log(numDocs/idfs.getOrElse(word, 1.0))
      }
      vec.zero()
      newWords.foreach{ case (index, value) =>
        vec(index) = value
      }
      //
    }
    //else {vec.activeElements.toMap}
  }

  def compressBags(ents:Iterable[Patent]) {
    ents.foreach{ ent =>
      trimBagTopK(ent.iprcLabel.features.value, 50)
    }
  }

  def trimBagTopK(vec:Tensor1, topK:Int) {
    if(vec.size > topK) {
      val topKItems = mutable.HashMap[Int, Double]()
      topKItems ++= vec.activeElements.toList.sortBy(_._2).reverse.take(topK)
      vec.zero()
      topKItems.foreach{ case (index, value) =>
        vec(index) = value
      }
    }
  }
}
