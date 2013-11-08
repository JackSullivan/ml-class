package so.modernized.experiments

import cc.factorie.variable._
import so.modernized.{PatentPipeline, Patent}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.strings
import cc.factorie.app.nlp.Document
import cc.factorie._
import cc.factorie.app.classify.OnlineLinearMultiClassTrainer

/**
 * Created with IntelliJ IDEA.
 * User: cellier
 * Date: 10/31/13
 * Time: 12:28 PM
 * To change this template use File | Settings | File Templates.
 */
class MaxEntExperiment {
  def train(patents:Iterable[Patent], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    var docLabels = new ArrayBuffer[LabelTag]()
    val trainVariables = patents.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }
    val testVariables = patents.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }
    println("Features Generated: Starting Training")
    PatentDomain.freeze()
    val classifier = new OnlineLinearMultiClassTrainer().train(docLabels.toSeq,docLabels.map(_.patent).toSeq)
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.patent.value).bestLabelIndex)(null))
    val objective = HammingObjective
    println ("Train accuracy = "+ objective.accuracy(trainVariables.toSeq))
    println ("Test  accuracy = "+ objective.accuracy(testVariables.toSeq))

  }

  object LabelDomain extends CategoricalDomain[String] {
    this ++= Vector("A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "N")
    freeze()
  }

  class PatentDescFeatures(patent:Patent) extends BinaryFeatureVectorVariable[String] {
    def domain = PatentDomain
    val label = new LabelTag(this,patent.sections.head)
    override def skipNonCategories = true
    strings.alphaSegmenter(patent.desc).foreach{token => this += token}
    strings.alphaSegmenter(patent.claims.reduce(_+_)).foreach{token => this += token}

    //println("Processing Patent: "+patent.id)
  }

  class LabelTag(val patent:PatentDescFeatures,labelString:String) extends LabeledCategoricalVariable(labelString) {
    def domain = LabelDomain
  }
  object PatentDomain extends CategoricalVectorDomain[String]

}

object MaxEntExperiment {
  def main(args: Array[String]){
    val MaxEnt = new MaxEntExperiment()
    MaxEnt.train(PatentPipeline("data/").toSeq)(random)


  }
}
