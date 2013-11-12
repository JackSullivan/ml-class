package so.modernized.experiments

import cc.factorie.variable._
import so.modernized.{PatentPipeline, Patent}
import cc.factorie.app.strings
import cc.factorie._
import cc.factorie.app.classify.BatchOptimizingLinearVectorClassifierTrainer

/**
 * User: cellier
 * Date: 10/31/13
 * Time: 12:28 PM
 */
class MaxEntExperiment {
  def train(patents:Iterable[Patent])(implicit random: scala.util.Random) {
    val docLabels = patents.map(patent => new PatentFeatures(patent).label)
    val (trainVariables, testVariables) = docLabels.shuffle.split(0.70)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    PatentDomain.freeze()
    val classifier = new BatchOptimizingLinearVectorClassifierTrainer().train[LabelTag, PatentFeatures](trainVariables, _.patent)
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.patent.value).bestLabelIndex)(null))
    val objective = HammingObjective
    println ("Train accuracy = "+ objective.accuracy(trainVariables))
    println ("Test  accuracy = "+ objective.accuracy(testVariables))


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

  class PatentFeatures(patent:Patent) extends BinaryFeatureVectorVariable[String] {
    def domain = PatentDomain
    val label = new LabelTag(this,patent.sections.head)
    override def skipNonCategories = true
    //strings.alphaSegmenter(patent.abs).foreach{token => this += token}
    strings.alphaSegmenter(patent.desc).foreach{token => this += token}
    //strings.alphaSegmenter(patent.claims.reduce(_+_)).foreach{token => this += token}
    //strings.alphaSegmenter(patent.title).foreach{token => this += token}
    //println("Processing Patent: "+patent.id)
  }

  class LabelTag(val patent:PatentFeatures,labelString:String) extends LabeledCategoricalVariable(labelString) {
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
