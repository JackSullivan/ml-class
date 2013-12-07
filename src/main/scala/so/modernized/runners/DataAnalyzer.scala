package so.modernized.runners

import so.modernized.{Patent, PatentPipeline}
import cc.factorie._
import scala.collection.mutable
import cc.factorie.la.Tensor1
import cc.factorie.Tensor1
import java.io.{FileWriter, BufferedWriter, File}

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
    val train = patents.toList
    println(train.length)
    val groupedPatents = train.groupBy(_.iprcSections.head)
    var sum = 0
    groupedPatents.foreach{
            case (label,classPatents) =>
              println(label + ": " + classPatents.size)
              sum += classPatents.size
    }
    groupedPatents.foreach{
      case (label,classPatents) =>
        println(label + ": " + classPatents.size/sum.toDouble)
    }
//    val tfidfVals = preparetfidf(train)
//    val coherenceFile =new BufferedWriter(new FileWriter("trueCategorytopics"))
//    //val coherencefile = new java.io.PrintWriter(new File("trueCategoryTopics.rtf"))
//    tfidfVals.foreach{ case tfidfVal=>
//      val buffer = new StringBuffer
//      buffer.append("topic" + tfidfVal._1+"\t")
//      tfidfVal._2.foreachActiveElement{ case (index, value) =>
//        buffer.append(Patent.FeatureDomain._dimensionDomain.dimensionName(index)+"\t")
//      }
//      coherenceFile.write(buffer.toString)
//      coherenceFile.write("\n")
//    }
//    coherenceFile.close()
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

  def preparetfidf(patents:Iterable[Patent]):Map[String,Tensor1] ={
    val otherVecs = patents.groupBy(_.iprcSections.head).map{group => (group._1, group._2.map{_.iprcLabel.features.value})}
    val patentVecs = otherVecs.map{case (group, vectors) => (group,vectors.reduce(_+_))}
    println(patentVecs.size)
    val idfs = idfCounts(patentVecs.unzip._2)
    println("generated idf counts")
    val counts = patentVecs.size
    patentVecs.foreach{patentVec => tfidf(patentVec._2, idfs, counts); trimBagTopK(patentVec._2,50)}

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
      trimBagTopK(ent.iprcLabel.features.value,50)
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
