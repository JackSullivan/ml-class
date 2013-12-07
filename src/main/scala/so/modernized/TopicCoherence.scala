package so.modernized

import java.util.HashMap
import scala.collection._
import java.io.{FileWriter, BufferedWriter, File}
import scala.math._
import scala.Predef._
import cc.factorie.variable.CategoricalSeqDomain
import so.modernized.runners.DataAnalyzer


object TopicCoherence {
  def main(args:Array[String]){
    val dataDir = args(0)
    val patents = PatentPipeline(dataDir).take(40000).toIterable
    implicit val random = scala.util.Random
    val tfidfVals = DataAnalyzer.preparetfidf(patents)

    val catTopics =new BufferedWriter(new FileWriter("trueCategorytopics.txt"))
    //val coherencefile = new java.io.PrintWriter(new File("trueCategoryTopics.rtf"))
    tfidfVals.foreach{ tfidfVal=>
      val buffer = new StringBuffer
      buffer.append("topic" + tfidfVal._1+"\t")
      tfidfVal._2.foreachActiveElement{ case (index, value) =>
        buffer.append(Patent.FeatureDomain._dimensionDomain.dimensionName(index)+"\t")
      }
      catTopics.write(buffer.toString)
      catTopics.write("\n")
    }
    catTopics.close()
    val topicfile = "trueCategorytopics.txt"
    val wordcount =args(2).toInt
    val output = args(3)
    val topic_highfrequencyword_list = generateHighTopicFrequenciesList(topicfile, wordcount)
    val (coherence,average) = runAnalysisAndCoherenceTesting(wordcount,patents,topic_highfrequencyword_list)
    val coherencefile = new java.io.PrintWriter(new File(output))
    coherence.foreach{
      case (topic,coherenceValue) =>
        coherencefile.write(topic+" "+coherenceValue+"\n")
    }
    coherencefile.write("Average = "+average+"\n")
    coherencefile.close()
    println(coherence)



  }

  def generateHighTopicFrequenciesList(fileName: String, wordcount: Int):mutable.HashMap[String,Vector[String]] ={
    val topic_highfrequencyword_list = new mutable.HashMap[String,Vector[String]]
    println("Reading topic file")
    val topic_words_file = scala.io.Source.fromFile(new File(fileName))
    for (line <- topic_words_file.getLines().zipWithIndex) {
      println("Line Read")
      val topic_words = line._1.split("\t")
      //var topWord:Vector[String] = Vector()

     // val topWords = topic_words.slice(1,wordcount+1)
      //val topWord:Vector[String] = Vector()
    //  topWords.foldLeft[Vector[String]](topWord){_ :+ _}
    //  assert(!topWord.isEmpty)
//      topWords.foreach{word =>  topWord.:+(word)}
//
//      //println(topword)
//      var topword:Vector[String] = Vector()
//      for(word <- topWords){
//        topword+=word
//      }

      topic_highfrequencyword_list("topic"+line._2) = topic_words.slice(1,wordcount+1).toVector.filterNot(_.startsWith("topic"))//.filterNot(_.split(' ').length > 1)

    }
    topic_words_file.close()
    topic_highfrequencyword_list
  }
  def tokenRegex = "\\p{Alpha}+"


  def runAnalysisAndCoherenceTesting(wordcount: Int,patents: Iterable[Patent],topic_highfrequencyword_list: mutable.HashMap[String,Vector[String]]):(mutable.HashMap[String,Double],Double)={
     var count = 0
     val numtopics = topic_highfrequencyword_list.size

     println("Intializing term-document frequencies")
     var types = scala.collection.mutable.Set[String]()
     val document_frequencies = new HashMap[String,Int]
     val codocument_frequencies = new HashMap[String,Int]
     var type_pairs =  scala.collection.mutable.Set[String]()

     //Initializing term document frequencies
     for(i<- 0 to numtopics-1){
      for(term <- topic_highfrequencyword_list.apply("topic"+i)){

        if(!document_frequencies.containsKey(term)){
          types += term
          document_frequencies.put(term,0)
        }
      }
     }
    println("Types "+types)
    // Initialising term pair document frequencies
    println("Initialising term pair document frequencies")
    for(i <- 0 to numtopics-1){
      for(m <- 1 to wordcount-2){
       val vm =topic_highfrequencyword_list.get("topic"+i).get(m)
       for(l<- 0 to m-1){
          val vl = topic_highfrequencyword_list.apply("topic"+i)(l)
          if((!codocument_frequencies.containsKey(vm+" "+vl)) && (!codocument_frequencies.containsKey(vl+" "+vm))){
            type_pairs += vm+" "+vl
            codocument_frequencies.put(vm+" "+vl,0)
          }
       }
      }
    }

    println("Document frequencies " +document_frequencies)
    println("Co-document frequencies "+codocument_frequencies)
    println("Reading documents")
    implicit object WordDomain extends CategoricalSeqDomain[String]

     patents.foreach{patent=>
     count=count+1
     //println("Line count :" +count)
      //val doc = patent.asLDADocument

     val doc_hash = new HashMap[String,Int]
     val docWords = patent.iprcLabel.features.activeCategories
//      for(type_values <- docWords) {
//        if(!doc_hash.containsKey(type_values)){
//            doc_hash.put(type_values,0)
//        }
//      }

       for(vl<-types) {
        if(docWords.contains(vl)){
          val newcount = document_frequencies.get(vl)+1
          //println("Reached")
          document_frequencies.put(vl,newcount)
        }
       }

       for(pairs <- type_pairs){
         //println(pairs)
         val pair = pairs.split(" ")
         //println(pair(0))
         if(docWords.contains(pair(0)) && docWords.contains(pair(1))){
           //println("Reached2")
           val newcount = codocument_frequencies.get(pairs)+1
           codocument_frequencies.put(pairs,newcount)
         }
       }

    }
    println("Document frequencies " +document_frequencies)
    println("Co-document frequencies "+codocument_frequencies)




    val coherence = mutable.HashMap[String,Double]()
    var sum=0.0

    for(i <- 0 to numtopics-1){
       var coherencevalue:Double = 0.0

       for(m <- 1 to wordcount-1){
         val vm =topic_highfrequencyword_list.apply("topic"+i)(m)
         for(l<- 0 to m-1){
             val vl = topic_highfrequencyword_list.apply("topic"+i)(l)

             //println(vm+" "+vl +" "+codocument_frequencies.get(vm+" "+vl))
             //println(vl +" "+document_frequencies.get(vl))

             val value = (codocument_frequencies.get(vm+" "+vl) + 1).toDouble/document_frequencies.get(vl).toDouble
             //println(value)
             //coherencevalue +=  log((codocument_frequencies.get(vm+" "+vl) + 1)/document_frequencies.get(vl))
             coherencevalue +=  log(value)
             //coherence.put("topic"+i)
           }
         }
        sum += coherencevalue
        coherence.put("topic"+i,coherencevalue)

        println("topic"+i,coherencevalue)
        }




    (coherence,sum.toDouble/numtopics.toDouble)

 }
}

//val sampleString = "This is a test document"
/*val doc1 = new cc.factorie.app.nlp.Document(line)
BasicTokenizer.process(doc1)
val doc = new StringBuilder()

for (token <- doc1.tokens) {
     doc.append(token.string+" ")
     //println(token)

}  */
