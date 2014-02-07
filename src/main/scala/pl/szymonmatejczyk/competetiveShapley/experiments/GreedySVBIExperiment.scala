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

object GreedySVBIExperiment extends App with Logging {
  val WEIGHT_DENOMINATOR = 10000L
  val MAX_GRAPH_SIZE = 500

  val LDAG_THRESHOLD = 1.0 / 320.0

  val BISV_ITER_NO = 20

  class ExperimentCase(val name: String, val network: InfluenceNetwork)

  class DataCase(name: String, val file: String, val filetype: FileType,
    val withWeights: Boolean = false)
    extends ExperimentCase(name, InfluenceNetwork.fromFile(file, filetype, withWeights).
        restrictSize(MAX_GRAPH_SIZE))

  class GeneratedCase(val generator: GraphGenerator, val size: Int) extends 
      ExperimentCase(generator.getClass().getSimpleName(),
    new InfluenceNetwork(generator.generateGraph(1 to size)))

  val TXT_PATH = "../graphs/txt/"
  val cases = Iterable( //new DataCase("football", "../graphs/gml/football.gml", GML),
    //    new DataCase("dolphins", "../graphs/gml/dolphins.gml", GML),
    //    new DataCase("polbooks", "../graphs/gml/polbooks.gml", GML),
    //    new DataCase("lesmiserables", "../graphs/gml/lesmiserables[W].gml", GML, true),
    new DataCase("amazon", TXT_PATH + "amazon0302.txt", TXT),
    new DataCase("p2pGnutella", TXT_PATH + "p2p-Gnutella04.txt", TXT),
    //        new DataCase("Slashdot", TXT_PATH + "Slashdot081106.txt", TXT),
    new DataCase("web-stanford", TXT_PATH + "web-Stanford.txt", TXT),
    new DataCase("wiki-Vote", TXT_PATH + "wiki-Vote.txt", TXT),
    new DataCase("email-Enron", TXT_PATH + "email-Enron.txt", TXT),
    //    new DataCase("simple.txt", TXT_PATH + "simple.txt", TXT),
    //    new GeneratedCase(new GeographicalThresholdGraphGenerator(0.9), 100),
    new GeneratedCase(new ErdosRandomGraphGenerator(0.3), 200),
    new DataCase("oregon1_010331.txt", TXT_PATH + "oregon1_010331.txt", TXT))

  val heapSize = Runtime.getRuntime().maxMemory();

  if (heapSize < 198974848)
    logger.warn(s"Max heap size($heapSize) may be to small.")

  class TestResult(val caseName: String, val seedSize: Int, val greedy: Double, val bi: Double,
    val sv: Double)
  val results = ListBuffer[TestResult]()

  def performExperiment(data: ExperimentCase, seedSize: Int) {
    println("Data: " + data.name + " Seed size: " + seedSize)
    val greedyResult = time(s"Greedy ${data.network.g.nodes.size} nodes:",
      data.network.greedyMostInfluentSearch(seedSize, Some(LDAG_THRESHOLD)))
    data.network.clearCache()
    val BIresult = time("BI: ", data.network.computeBIRanking(LDAG_THRESHOLD, BISV_ITER_NO).
      take(seedSize))
    data.network.clearCache()
    val SVresult = time("SV: ", data.network.computeSVRanking(BISV_ITER_NO, LDAG_THRESHOLD).
      take(seedSize))
    val greedySigma = data.network.computeTotalInfluence(greedyResult)
    println("Greedy: " + greedySigma)
    // println(greedyResult)
    val biSigma = data.network.computeTotalInfluence(BIresult.iterator.map(_._1).toIterable)
    println("BI: " + biSigma)
    //      println(BIresult)
    val svSigma = data.network.computeTotalInfluence(SVresult.iterator.map(_._1).toIterable)
    println("SV: " + svSigma)

    val gbiSimilarity = setSimilarity(greedyResult.toSet, BIresult.view.map(_._1).toSet)
    println(s"G:BI similarity: $gbiSimilarity")
    val gsvSimilarity = setSimilarity(greedyResult.toSet, SVresult.view.map(_._1).toSet)
    println(s"G:SV similarity: $gsvSimilarity")
    results += new TestResult(data.name, seedSize, greedySigma, biSigma, svSigma)
  }

  for (
    data <- cases;
    seedSize <- Iterator.range(10, 50, 5)
  ) {
    try {
      performExperiment(data, seedSize)
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

      print("seedSize")
      results.foreach {
        x => print("\t" + x.seedSize)
      }
      println()
      print("greedy")
      results.foreach {
        x => print("\t" + x.greedy)
      }
      println()
      print("bi")
      results.foreach {
        x => print("\t" + x.bi)
      }
      println()
      print("sv")
      results.foreach {
        x => print("\t" + x.sv)
      }
      writer.close()
  }
}