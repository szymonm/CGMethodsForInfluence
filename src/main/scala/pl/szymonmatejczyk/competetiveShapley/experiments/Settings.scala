package pl.szymonmatejczyk.competetiveShapley.experiments

import com.typesafe.config.Config
import java.net.InetAddress
import pl.szymonmatejczyk.competetiveShapley.InfluenceHeuristicForSequenceOfK
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader.{TXT, GML}
import pl.szymonmatejczyk.competetiveShapley.michalakGames.{InfluenceAboveThresholdGameSV, FringeGameSV}
import pl.szymonmatejczyk.competetiveShapley.randomGraphs.{ErdosRandomGraphGenerator, GeographicalThresholdGraphGenerator}
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms._
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.{LDAGShapleyValue, LDAGBanzhafIndex, ShapleyValueWithDiscount}
import scala.collection.JavaConversions._
import com.typesafe.scalalogging.LazyLogging

class Settings(config: Config) extends LazyLogging {

  val heapSize = java.lang.Runtime.getRuntime.maxMemory()
  val hostname = InetAddress.getLocalHost.getHostName

  if (heapSize < 198974848)
    logger.warn(s"Max heap size($heapSize) may be to small.")

  val resultsDirectory = config.getString("experiments.resultsDirectory") + hostname

//  val WEIGHT_DENOMINATOR = config.getLong("graph.weightDenominator")

  val LDAG_THRESHOLD = config.getDouble("graph.ldagThreshold")

  val BI_ITER_NO = config.getInt("graph.biIterNo")
  val SV_ITER_NO = config.getInt("graph.svIterNo")

  val MC_RUNS = config.getInt("graph.mcRuns")

  implicit val MAX_GRAPH_SIZE = GraphSizeRestriction(config.getInt("experiments.maxGraphSize"))

  def getFileName(path: String) = path.split("/").last.split("\\.").head

  private val unweightedGML = config.getStringList("experiments.smallCases.unweighted.gml")
    .map(x => DataCase(getFileName(x), x, GML))
  private val weightedGML = config.getStringList("experiments.smallCases.weighted.gml")
    .map(x => DataCase(getFileName(x), x, GML, withWeights = true))

  val smallCases = unweightedGML ++ weightedGML

  private val bigUnweightedTXT: Seq[DataCase] = config.getStringList("experiments.bigCases.unweighted.txt")
    .map(x => DataCase(getFileName(x), x, TXT))
  private val bigWeightedGML: Seq[DataCase] = config.getStringList("experiments.bigCases.weighted.gml")
    .map(x => DataCase(getFileName(x), x, GML, withWeights = true))

  val bigCases = bigUnweightedTXT ++ bigWeightedGML

  val includeBigCases = config.getBoolean("experiments.includeBigCases")
  val includeSmallCases = config.getBoolean("experiments.includeSmallCases")

  val randomCases = List(
    new GeneratedCase(new GeographicalThresholdGraphGenerator(0.9), 100),
    new GeneratedCase(new ErdosRandomGraphGenerator(0.3), 200)
  )

  val others = List(DataCase("simple.txt", "../graphs/txt/simple.txt", TXT))

  val testedCases: Seq[ExperimentCase] = Seq[ExperimentCase]() ++
    (if (includeSmallCases) smallCases else List()) ++
    (if (includeBigCases) bigCases else List())

  val algorithms = List[InfluenceHeuristicForSequenceOfK](
    GreedyLDAGTopNodes.influenceHeuristicForSequenceOfK(LDAG_THRESHOLD),
    LDAGBanzhafIndex.influenceHeuristicForSequenceOfK(BI_ITER_NO, LDAG_THRESHOLD),
    LDAGShapleyValue.influenceHeuristicForSequenceOfK(SV_ITER_NO, LDAG_THRESHOLD),
    FringeGameSV.influenceHeuristicForSequenceOfK,
//    KFringeGameSV.influenceHeuristic(3),
//    DistanceCutoffGameSV.influenceHeuristic(1.0),
    InfluenceAboveThresholdGameSV.influenceHeuristicForSequenceOfK(1.0),
    CelfPlusPlus.influenceHeuristicForSequenceOfK,
    DegreeDiscount.influenceHeuristicForSequenceOfK,
    ShapleyValueWithDiscount.influenceHeuristicForSequenceOfK
  )

  val referenceHeuristic = RandomNodes.influenceHeuristicForSequenceOfK
}
