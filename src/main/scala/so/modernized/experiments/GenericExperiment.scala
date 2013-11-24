package so.modernized.experiments

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.app.classify._
import scala.util.Random
import scala.io.Source

/**
 * @author John Sullivan
 */
object GenericLabelDomain extends CategoricalDomain[String]
class GenericFeatureDomain(featList:Iterable[Any]) extends DiscreteDomain(featList)
class GenericLabel(label:String, featureList:Iterable[Double], featureDomain:GenericFeatureDomain) extends LabeledCategoricalVariable[String](label) {
  def domain = GenericLabelDomain
  def features = new VectorVariable(new DenseTensor1(featureList.toArray)){
      def domain: VectorDomain = featureDomain
    }
}

class GenericExperiment(trainer:LinearVectorClassifierTrainer, methodName:String) {
  def runExperiment(labels:Iterable[GenericLabel], testSplit:Double = 0.7)(implicit random:Random):ExperimentResult = {
    val (trainVariables, testVariables) = labels.shuffle.split(0.7)
    (trainVariables ++ testVariables).foreach(_.setRandomly)
    println("Features Generated: Starting Training")
    trainVariables.head.features.domain.freeze()

    val classifier = trainer.train(new LinearVectorClassifier(GenericLabelDomain.dimensionSize, trainVariables.head.features.domain.dimensionSize, {l:GenericLabel => l.features}), trainVariables, {l:GenericLabel => l.features})
    (trainVariables ++ testVariables).foreach(v => v.set(classifier.classification(v.features.value).bestLabelIndex)(null))

    val objective = HammingObjective
    ExperimentResult(methodName, objective.accuracy(trainVariables), objective.accuracy(testVariables))
  }
}

object GenericExperiment {

  def readCSV(filename:String):Iterable[GenericLabel] = {
    var fDomain:Option[GenericFeatureDomain] = None
    Source.fromFile(filename).getLines().map { line =>
      val cells = line.split(",")
      if(fDomain.isEmpty) {
        fDomain = Option(new GenericFeatureDomain(cells.tail))
      }
      new GenericLabel(cells.head, cells.tail.map{_.toDouble}, fDomain.get)
    }.toList
  }

  def main(args:Array[String]) {
    implicit val random = Random
    val isofiles = (Seq("compressed_data/ISOMap3.csv","compressed_data/ISOMap10.csv","compressed_data/ISOMap50.csv","compressed_data/ISOMap100.csv","compressed_data/ISOMap1000.csv"),"ISOMap")
    val laplacianfiles = (Seq("compressed_data/Laplacian3.csv","compressed_data/Laplacian10.csv","compressed_data/Laplacian50.csv","compressed_data/Laplacian100.csv","compressed_data/Laplacian1000.csv"),"Laplacian")
    val pcafiles = (Seq("compressed_data/PCA3.csv","compressed_data/PCA10.csv","compressed_data/PCA50.csv","compressed_data/PCA100.csv","compressed_data/PCA1000.csv"),"PCA")

    val out = Seq(isofiles,laplacianfiles,pcafiles).map{files => (files._2,files._1.map{
      file =>
        (new GenericExperiment(new BatchOptimizingLinearVectorClassifierTrainer()(random),"Maxent").runExperiment(readCSV(file)),
       // new GenericExperiment(new NaiveBayesClassifierTrainer(),"NaiveBayes").runExperiment(readCSV(file)),
        new GenericExperiment(new SVMLinearVectorClassifierTrainer()(random),"Svm").runExperiment(readCSV(file))
      )
    }) }.map(t=> (t._1,t._2.seq.unzip))
    out.foreach{case (compressionType,compressionResult) =>
      println(compressionType)
      compressionResult._1.foreach(t=> println(t.trainAccuracy + "," + t.testAccuracy))
      compressionResult._2.foreach(t=> println(t.trainAccuracy + "," + t.testAccuracy))
     // compressionType._3.foreach(t=> println(t.trainAccuracy + "," + t.testAccuracy))
    }


  }
}
