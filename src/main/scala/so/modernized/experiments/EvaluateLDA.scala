package so.modernized.experiments

import so.modernized.Patent
import so.modernized.Patent.Label

/*
 * User: cellier
 * Date: 11/24/13
 * Time: 12:46 PM
 */
class EvaluateLDA(patents: Iterable[Patent], numTopics: Int) {
  val patentTopicGroups = patents.groupBy(_.unsupervisedLabel).map{
    topic => new TopicResult(topic._1,topic._2,topic._2.groupBy(_.iprcLabel).mapValues(_.size.toDouble))
  }
  patentTopicGroups.foreach{
    topic => println("Topic " + topic.label.get + ":\n")
      topic.officialGroupCounts.foreach{ case (offGroup,count) => println("Official Group: " + offGroup + " = " + count)}
  }
  patentTopicGroups.foreach{topic => println("Accuracy " + topic.label.get + ": " + topic.officialGroupCounts.maxBy(_._2)._2/topic.patents.size)}
}
case class TopicResult(label: Option[Label], patents:Iterable[Patent], officialGroupCounts: Map[Patent.Label,Double])
