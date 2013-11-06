package so.modernized.experiments

import cc.factorie.app.nlp.parse.TransitionParser
import cc.factorie.optimize.{Trainer, AdaGradRDA}
import cc.factorie.app.nlp.Sentence
import cc.factorie.variable._
import so.modernized.{PatentPipeline, Patent}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.strings
import cc.factorie.app.classify.OnlineLinearMultiClassTrainer
import cc.factorie._

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
  def train(trainPatents:Seq[Patent], testPatents:Seq[Patent], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {

    var docLabels = new ArrayBuffer[Label]()
    val trainVariables = trainPatents.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }
    val testVariables = testPatents.flatMap{ patent => docLabels += new PatentDescFeatures(patent).label }

    PatentDomain.freeze()
    val classifier = new OnlineLinearMultiClassTrainer().train(trainVariables,trainVariables.map(_.patent))
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.patent.value).bestLabelIndex)(null))
    val objective = HammingObjective
    println ("Train accuracy = "+ objective.accuracy(trainVariables))
    println ("Test  accuracy = "+ objective.accuracy(testVariables))
  /*
  def evaluate() {

    //(trainVariables ++ testVariables).foreach(s => model.maximize(s.tokens.map(_.posLabel))(null))
    //println("Train accuracy: "+ HammingObjective.accuracy(trainVariables.flatMap(s => s.tokens.map(_.posLabel))))
      //println("Test accuracy: "+ HammingObjective.accuracy(testPatents.flatMap(s => s.tokens.map(_.posLabel))))
    }
    val examples = trainVariables.map(patent => new model.ChainStructuredSVMExample(sentence.tokens.map(_.posLabel))).toSeq
    //val optimizer = new cc.factorie.optimize.AdaGrad(rate=lrate)
    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = false)
      */
  }

  object LabelDomain extends CategoricalDomain[String] {
    this ++= Vector("A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H")
    freeze()
  }
  class PatentDescFeatures(patent:Patent) extends BinaryFeatureVectorVariable[String] {
    def domain = PatentDomain
    val label = new Label(patent.section, this)
    strings.alphaSegmenter(patent.desc).foreach(token => this += token)
  }
  /*
  class PatentClaimsFeatures(patent:Patent) extends BinaryFeatureVectorVariable[String] {
    def domain = PatentDomain
    val label = new Label(patent.section, this)
    patent.claims.map{claim => strings.alphaSegmenter(claim).foreach(token => this += token)}
  }  */

  class Label(labelString:String, val patent:PatentDescFeatures) extends LabeledCategoricalVariable(labelString) {
    def domain = LabelDomain
  }

  object PatentDomain extends CategoricalVectorDomain[String]

}

object MaxEntExperiment {
  def main(args: Array[String]){
    val patents = PatentPipeline("data/")
    val trainPatents = patents.drop(patents.length/2)
    val testPatents = patents
    val MaxEnt = new MaxEntExperiment()
    MaxEnt.train(trainPatents.toSeq,testPatents.toSeq)(random)
  }
}
