package pl.szymonmatejczyk.competetiveShapley.experiments

import java.io.PrintWriter
import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import com.typesafe.scalalogging.slf4j.Logging
import pl.szymonmatejczyk.competetiveShapley.utils._
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils.time
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader._
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader
import pl.szymonmatejczyk.competetiveShapley.randomGraphs.ErdosRandomGraphGenerator
import pl.szymonmatejczyk.competetiveShapley.randomGraphs.GraphGenerator
import pl.szymonmatejczyk.competetiveShapley.randomGraphs.GeographicalThresholdGraphGenerator
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.michalakGames.FringeGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.DistanceCutoffGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.InfluenceAboveThresholdGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.DistanceCutoffGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.KFringeGameSV
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.CelfPlusPlus
import java.net.InetAddress
import scala.util.Success
import scala.util.Failure
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.RandomNodes
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.ShapleyValueWithDiscount
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.DegreeDiscount
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.GreedyLDAGTopNodes
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.LDAGShapleyValue
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.LDAGBanzhafIndex

object GreedySVBIExperiment extends App with Logging {
  val heapSize = java.lang.Runtime.getRuntime().maxMemory();
  val hostname = InetAddress.getLocalHost().getHostName()

  if (heapSize < 198974848)
    logger.warn(s"Max heap size($heapSize) may be to small.")
    
  val WEIGHT_DENOMINATOR = 10000L
  
  val LDAG_THRESHOLD = 1.0 / 320.0

  val BI_ITER_NO = 1000
  val SV_ITER_NO = 200
  
  val MC_RUNS = 10000
  
  def SEED_PERCENT_RANGE = Range(2, 32,  4)
  
  val resultsDirectory = if (args.size > 0) args(0) else s"results$hostname"
  (new File(resultsDirectory)).mkdir()
  
  val allResults = new PrintWriter(new File(resultsDirectory + "/all.res"))
  
  val MAX_GRAPH_SIZE = if (args.size > 1) args(1).toInt else 2000

  class ExperimentCase(val name: String, val network: InfluenceNetwork)

  class DataCase(name: String, val file: String, val filetype: FileType,
    val withWeights: Boolean = false)
    extends ExperimentCase(name, InfluenceNetwork.fromFile(file, filetype, withWeights).
        restrictSize(MAX_GRAPH_SIZE))

  class GeneratedCase(val generator: GraphGenerator, val size: Int) 
      extends ExperimentCase(generator.getClass().getSimpleName(),
    new InfluenceNetwork(generator.generateGraph[Int](1 to size)))
  
  val smallCases = List(new DataCase("football", "../graphs/gml/football.gml", GML),
        new DataCase("dolphins", "../graphs/gml/dolphins.gml", GML),
        new DataCase("polbooks", "../graphs/gml/polbooks.gml", GML),
        new DataCase("lesmiserables", "../graphs/gml/lesmiserables[W].gml", GML, true)
      )
  
  val bigCases = List(
    new DataCase("amazon", TXT_PATH + "amazon0302.txt", TXT),
    new DataCase("p2pGnutella", TXT_PATH + "p2p-Gnutella04.txt", TXT),
//    new DataCase("Slashdot", TXT_PATH + "Slashdot081106.txt", TXT),
    new DataCase("web-stanford", TXT_PATH + "web-Stanford.txt", TXT),
    new DataCase("hep-th", "../graphs/gml/hep-th[W].gml", GML, true)
//    new DataCase("wiki-Vote", TXT_PATH + "wiki-Vote.txt", TXT),
//    new DataCase("email-Enron", TXT_PATH + "email-Enron.txt", TXT)
//    new DataCase("oregon1_010331.txt", TXT_PATH + "oregon1_010331.txt", TXT)
    )
  
  val randomCases = List(
    new GeneratedCase(new GeographicalThresholdGraphGenerator(0.9), 100),
    new GeneratedCase(new ErdosRandomGraphGenerator(0.3), 200)
    )
  
  val others = List(new DataCase("simple.txt", TXT_PATH + "simple.txt", TXT))

  val TXT_PATH = "../graphs/txt/"
  val cases = List[DataCase]()

  import pl.szymonmatejczyk.competetiveShapley.common._
  
  type IN = InfluenceNetwork
  val greedyLDAGHeuristic = GreedyLDAGTopNodes.influenceHeuristicForSequenceOfK(LDAG_THRESHOLD)

  val heuristics = List(
      LDAGBanzhafIndex.influenceHeuristicForSequenceOfK(BI_ITER_NO, LDAG_THRESHOLD),
      LDAGShapleyValue.influenceHeuristicForSequenceOfK(SV_ITER_NO, LDAG_THRESHOLD),
      FringeGameSV.influenceHeuristicForSequenceOfK,
//      KFringeGameSV.influenceHeuristic(3),
//      DistanceCutoffGameSV.influenceHeuristic(1.0),
      InfluenceAboveThresholdGameSV.influenceHeuristicForSequenceOfK(1.0),
      CelfPlusPlus.influenceHeuristicForSequenceOfK,
      RandomNodes.influenceHeuristicForSequenceOfK,
      ShapleyValueWithDiscount.influenceHeuristicForSequenceOfK,
      DegreeDiscount.influenceHeuristicForSequenceOfK
      )
  
  case class TestValue(val ICvalue : Double, time : Double, greedySimilarity : Double, LTvalue : Double)
  
  case class TestResult(val caseName: String, val values : mutable.Map[(String, Int), TestValue]) {
    def this(name : String) = this(name, mutable.Map[(String, Int), TestValue]())
  }

  val results = ListBuffer[TestResult]()

  def performExperiment(data: ExperimentCase, seedSizes: Seq[Int]) 
      : Future[TestResult] = future {
    println("Data: " + data.name + " Seed sizes: " + seedSizes.mkString(","))
    val net = data.network
    val res = new TestResult(data.name)
    
    logger.info(s"Testing greedy (${data.name})")
    val greedyResults = greedyLDAGHeuristic._2(data.network)(seedSizes)
    data.network.clearCache()
    val greedyTimes = greedyResults.map(_._2)
    val greedyICQualities  = greedyResults.map(x => net.computeTotalInfluence(x._1))
    val greedyLTQualities = greedyResults.map(x => net.mcLinearThresholdSeedQuality(x._1, MC_RUNS))
    
    val tvs = greedyTimes.zip(greedyICQualities).zip(greedyLTQualities).map(
        x => new TestValue(x._1._2, x._1._1.toMillis, 1.0, x._2))
    
    res.values ++= seedSizes.map((greedyLDAGHeuristic._1, _)).zip(tvs)
    seedSizes.map((greedyLDAGHeuristic._1, _)).zip(tvs).foreach{
      x => allResults.println(s"${x._1._1} ${x._1._2} ${x._2.ICvalue} ${x._2.time} " + 
          s"${x._2.greedySimilarity} ${x._2.LTvalue}")
    }
    allResults.flush()
     
    data.network.clearCache()
    greedyICQualities.zip(greedyTimes).foreach{
      x => logger.info(s"Greedy(${data.name}): ${x._1} time: ${x._2}")
    }
    
    for (heuristic <- heuristics) {
      logger.info(s"Testing ${heuristic._1}(${data.name})")
      val results = heuristic._2(data.network)(seedSizes)
      data.network.clearCache()
      val times = results.map(_._2)
      val ICqualities = results.map(x => net.computeTotalInfluence(x._1))
      val LTqualities = results.map(x => net.mcLinearThresholdSeedQuality(x._1, MC_RUNS))
      val similarities = results.map(_._1).zip(greedyResults.map(_._1)).map(
          x => setSimilarity(x._1.toSet, x._2.toSet))

      val tvs = (ICqualities.zip(times)).zip(similarities.zip(LTqualities)).map(
        x => new TestValue(x._1._1, x._1._2.toMillis, x._2._1, x._2._2))
      seedSizes.map((heuristic._1, _)).zip(tvs).foreach {
        x =>
          allResults.println(s"${x._1._1} ${x._1._2} ${x._2.ICvalue} ${x._2.time} " +
            s"${x._2.greedySimilarity} ${x._2.LTvalue}")
      }
      allResults.flush()
      res.values ++= seedSizes.map((heuristic._1, _)).zip(tvs)
      data.network.clearCache()
      ICqualities.zip(times).foreach {
        x => logger.info(s"${heuristic._1}(${data.name}): ${x._1} time: ${x._2}")
      }
    }
    res
  }

  val futures = for (
    data <- cases
  ) yield {
    val future = performExperiment(data, SEED_PERCENT_RANGE.map(_ * data.network.size / 100))
    future.onFailure {
          case e: Throwable =>
            logger.warn(s"Experiment ${data.name} failed.")
            logger.warn(e.getMessage())
            logger.warn(e.getStackTraceString)
    }
    future.onSuccess {
      case result =>
        logger.info(s"Printing results ${data.name}")
        val filename = data.name.replace(".", "_")
        printResultsToFile(s"$resultsDirectory/$filename.res",
          result)
        printResultsToFile(s"$resultsDirectory/${filename}_times.res",
          result, (_.time))
        printResultsToFile(s"$resultsDirectory/${filename}_greedySimilarities.res",
          result, (_.greedySimilarity))
        printResultsToFile(s"$resultsDirectory/${filename}_lt.res",
          result, (_.LTvalue))
    }
    future
  }
  
  Await.result(Future.sequence(futures), Duration.Inf)
  allResults.close()

  def printResultsToFile(filename : String, result : TestResult, 
      unpack : TestValue => Double = (_.ICvalue)) {
    val seedSizesSorted = result.values.keys.map(_._2).toList.sorted
    val writer = new PrintWriter(new File(filename))
    
    def aligningSpaces(s : String) = " " * (20 - s.size)
    def stringAligned(s : String) = s + aligningSpaces(s)
    writer.println(s"${stringAligned("seedSize")}\t" + seedSizesSorted.mkString("\t"))
    (greedyLDAGHeuristic :: heuristics).foreach{
      heuristic =>
        val resultsSorted = seedSizesSorted.map(x => unpack(result.values((heuristic._1, x))))
          .mkString("\t")
        writer.println(stringAligned(heuristic._1) + "\t" + resultsSorted)
    }
    writer.close()
  }
}