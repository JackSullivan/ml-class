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
//  def evaluateParameters(args:Array[String]): Double = {
//    val numBootstrappingIterations = //opts.bootstrapping.value.toInt
//    val c = new TransitionParser
//    val l1 = 2*opts.l1.value / //sentences.length
//    val l2 = 2*opts.l2.value / //sentences.length
//    val optimizer = new AdaGradRDA(opts.rate.value, opts.delta.value, l1, l2)
//
//    var patentClasses = new ArrayBuffer[Label]()
//  }

  def train(patents:Iterator[Patent], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    var docLabels = new ArrayBuffer[LabelTag]()
    val trainVariables = patents.toList.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }
    val testVariables = patents.toList.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }

    //val trainVariables = createDocuments(400,patents)
    //val testVariables = createDocuments(10,patents)
    println("Features Generated: Starting Training")
    PatentDomain.freeze()
//    def evaluate() {
//      model.maximize((trainVariables++testVariables).map(_.attr[LabelTag]))(null)
//      println("Train accuracy: "+ HammingObjective.accuracy(trainVariables.map(_.attr[LabelTag])))
//      println("Test accuracy: "+ HammingObjective.accuracy(testVariables.map(_.attr[LabelTag])))
//    }
//    val examples = trainVariables.groupBy(d=> d.attr[LabelTag]).map(doc => new model.ChainStructuredSVMExample(trainVariables.map(_.attr[LabelTag]))).toSeq
//    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
//    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = false)

    val classifier = new OnlineLinearMultiClassTrainer().train(docLabels.toSeq,trainVariables.map(_.patent).toSeq)
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
    strings.alphaSegmenter(patent.desc).foreach{token => this + token}
    strings.alphaSegmenter(patent.claims.reduce(_+_)).foreach{token => this + token}

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
    MaxEnt.train(PatentPipeline("data/"))(random)


  }
}
