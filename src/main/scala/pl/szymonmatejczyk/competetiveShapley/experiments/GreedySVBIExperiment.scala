package pl.szymonmatejczyk.competetiveShapley.experiments

import java.io.PrintWriter
import java.io.File
import scala.collection._
import scala.collection.mutable.ListBuffer
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

object GreedySVBIExperiment extends App with Logging {
  val heapSize = Runtime.getRuntime().maxMemory();

  if (heapSize < 198974848)
    logger.warn(s"Max heap size($heapSize) may be to small.")
    
  val WEIGHT_DENOMINATOR = 10000L
  val MAX_GRAPH_SIZE = 300

  val LDAG_THRESHOLD = 1.0 / 320.0

  val BISV_ITER_NO = 100

  class ExperimentCase(val name: String, val network: InfluenceNetwork)

  class DataCase(name: String, val file: String, val filetype: FileType,
    val withWeights: Boolean = false)
    extends ExperimentCase(name, InfluenceNetwork.fromFile(file, filetype, withWeights).
        restrictSize(MAX_GRAPH_SIZE))

  class GeneratedCase(val generator: GraphGenerator, val size: Int) extends 
      ExperimentCase(generator.getClass().getSimpleName(),
    new InfluenceNetwork(generator.generateGraph(1 to size)))

  val TXT_PATH = "../graphs/txt/"
  val cases = Iterable(new DataCase("football", "../graphs/gml/football.gml", GML),
        new DataCase("dolphins", "../graphs/gml/dolphins.gml", GML),
        new DataCase("polbooks", "../graphs/gml/polbooks.gml", GML),
        new DataCase("lesmiserables", "../graphs/gml/lesmiserables[W].gml", GML, true),
        new DataCase("amazon", TXT_PATH + "amazon0302.txt", TXT),
        new DataCase("p2pGnutella", TXT_PATH + "p2p-Gnutella04.txt", TXT),
    //        new DataCase("Slashdot", TXT_PATH + "Slashdot081106.txt", TXT),
//    new DataCase("web-stanford", TXT_PATH + "web-Stanford.txt", TXT),
//    new DataCase("wiki-Vote", TXT_PATH + "wiki-Vote.txt", TXT),
//    new DataCase("email-Enron", TXT_PATH + "email-Enron.txt", TXT),
    //    new DataCase("simple.txt", TXT_PATH + "simple.txt", TXT),
    //    new GeneratedCase(new GeographicalThresholdGraphGenerator(0.9), 100),
//    new GeneratedCase(new ErdosRandomGraphGenerator(0.3), 200),
    new DataCase("oregon1_010331.txt", TXT_PATH + "oregon1_010331.txt", TXT))

  import pl.szymonmatejczyk.competetiveShapley.common._
  
  type IN = InfluenceNetwork
  val greedyLDAGHeuristic = new InfluenceHeuristic("greedyLdag", (in : IN) => (k : Int) => 
        in.greedyMostInfluentSearch(k, Some(LDAG_THRESHOLD)))

  val heuristics = List(
      new InfluenceHeuristic("ldagBI", (in : IN) => (k : Int) => 
        in.computeBIRanking(LDAG_THRESHOLD, BISV_ITER_NO).take(k).map(_._1)),
      new InfluenceHeuristic("ldagSV", (in : IN) => (k : Int) => in.computeSVRanking(BISV_ITER_NO,
          LDAG_THRESHOLD).take(k).map(_._1)),
      FringeGameSV.influenceHeuristic,
      KFringeGameSV.influenceHeuristic(3),
      DistanceCutoffGameSV.influenceHeuristic(1.0),
//      InfluenceAboveThresholdGameSV.influenceHeuristic(1.0),
      CelfPlusPlus.influenceHeuristic(10000)
      )
  
  type TestValue = (Double, Double, Double) // (value, time, greedySimilarity
  
  class TestResult(val caseName: String, val seedSize: Int, val values : Map[String, TestValue])

  val results = ListBuffer[TestResult]()

  def performExperiment(data: ExperimentCase, seedSize: Int) {
    println("Data: " + data.name + " Seed size: " + seedSize + "%")
    val values = mutable.Map[String, TestValue]()
    
    logger.info(s"Testing greedy")
    val (greedyResult, greedyTime) = time(greedyLDAGHeuristic._2(data.network)(seedSize))
    val greedyQuality  = data.network.computeTotalInfluence(greedyResult)
    values += ((greedyLDAGHeuristic._1, (greedyQuality, greedyTime.toMillis, 1.0)))
    data.network.clearCache()
    logger.info(s"Greedy: $greedyQuality, $greedyTime")
    
    for (heuristic <- heuristics) {
      logger.info(s"Testing ${heuristic._1}")
      val (result, runningTime) = time(heuristic._2(data.network)(seedSize))
      data.network.clearCache()
      val quality = data.network.computeTotalInfluence(result)
      val greedySimilarity = setSimilarity(greedyResult.toSet, result.toSet)
      values += ((heuristic._1, (quality, runningTime.toMillis, greedySimilarity)))
      data.network.clearCache()
      logger.info(s"${heuristic._1}: $quality, $runningTime, $greedySimilarity")
    }
    
    results += new TestResult(data.name, seedSize, values)
  }

  for (
    data <- cases;
    seedSize <- Iterator.range(10, 50, 5)
  ) {
    try {
      performExperiment(data, data.network.size * seedSize / 100)
    } catch {
      case e: Throwable =>
        println(s"Experiment: ${data.name}:$seedSize failed")
        e.printStackTrace()
    }
  }

  val resultsByName = results.groupBy(_.caseName)
  resultsByName.foreach {
    case (caseName, results) =>
      val writer = new PrintWriter(new File("results/" + caseName.replace(".", "_") + ".res"))
      def print(x: Any) {
        writer.print(x)
      }
      def println() {
        writer.println()
      }

      print("seedSize (%)")
      results.foreach {
        x => print("\t" + x.seedSize)
      }
      println()
      for (heuristic <- (greedyLDAGHeuristic :: heuristics)) {
        print(heuristic._1)
        results.foreach {
          x => print("\t" + x.values(heuristic._1))
        }
      }
      writer.close()
  }
}