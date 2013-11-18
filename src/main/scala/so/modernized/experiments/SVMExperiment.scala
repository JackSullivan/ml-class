package so.modernized.experiments

import cc.factorie.Factorie.CategoricalDomain
import so.modernized.{PatentPipeline, Patent}
import cc.factorie.variable.{HammingObjective, CategoricalVectorDomain, LabeledCategoricalVariable, BinaryFeatureVectorVariable}
import cc.factorie.app.strings
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.classify.{SVMLinearVectorClassifierTrainer, LinearVectorClassifier}
import cc.factorie._

/**
 * User: cellier
 * Date: 11/14/13
 * Time: 7:26 PM
 */
class SVMExperiment{

  def train(patents:Iterable[Patent])(implicit random: scala.util.Random) {
    var docLabels = new ArrayBuffer[LabelTag]()
    patents.foreach{ patent => docLabels += new PatentFeatures(patent).label }
    val (trainVariables, testVariables) = docLabels.shuffle.split(0.70)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    PatentDomain.freeze()
    val classifier = new LinearVectorClassifier[LabelTag, PatentFeatures](LabelDomain.dimensionSize, PatentDomain.dimensionSize, _.patent)
    new SVMLinearVectorClassifierTrainer().train[LinearVectorClassifier[LabelTag, PatentFeatures], LabelTag, PatentFeatures](classifier, trainVariables, {l:LabelTag => l.patent})

    //new SVMLinearVectorClassifierTrainer().train[LabelTag, PatentFeatures]()
    /*
    def evaluate(cls: LinearMultiClassClassifier) {
      println(cls.weights.value.toSeq.count(x => x == 0).toFloat/cls.weights.value.length +" sparsity")
    }
    lazy val model = new LinearMultiClassClassifier(LabelDomain.size, PatentDomain.dimensionSize)
    val svm = new SVMMultiClassTrainer()
    svm.baseTrain(model,trainVariables.toSeq.map(_.labelInt),trainVariables.map(_.patent.value).toSeq,trainVariables.map(w => 1.0).toSeq,evaluate)// (trainVariables.toSeq,trainVariables.map(_.patent).toSeq)
    */
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

  class PatentFeatures(patent:Patent) extends BinaryFeatureVectorVariable[String] {
    def domain = PatentDomain
//    val labelInt = patent.sections.head match {
//      case "A" => 1
//      case "B" => 2
//      case "C" => 3
//      case "D" => 4
//      case "E" => 5
//      case "F" => 6
//      case "G" => 7
//      case "H" => 8
//    }
    val label = new LabelTag(this,patent.sections.head)
    override def skipNonCategories = true
    //strings.alphaSegmenter(patent.abs).foreach{token => this += token}
    strings.alphaSegmenter(patent.desc).foreach{token => this += token}
    //strings.alphaSegmenter(patent.claims.reduce(_+_)).foreach{token => this += token}
    //strings.alphaSegmenter(patent.title).foreach{token => this += token}
    //println("Processing Patent: "+patent.id)
  }

  class LabelTag(val patent:PatentFeatures,labelString:String) extends LabeledCategoricalVariable(labelString) {
    val labelInt = labelString match {
      case "A" => 1
      case "B" => 2
      case "C" => 3
      case "D" => 4
      case "E" => 5
      case "F" => 6
      case "G" => 7
      case "H" => 8
      case "N" => 9
    }
    def domain = LabelDomain
  }
  object PatentDomain extends CategoricalVectorDomain[String]

}

object SVMExperiment {
  def main(args: Array[String]){
    val svm = new SVMExperiment()
    svm.train(PatentPipeline("data/").toList)(random)


  }
}

