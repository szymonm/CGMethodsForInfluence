package pl.szymonmatejczyk.competetiveShapley.experiments

import java.io.PrintWriter
import java.io.File
import java.nio.file.Files
import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import com.typesafe.scalalogging.slf4j.Logging
import pl.szymonmatejczyk.competetiveShapley.utils._
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils.time
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader
import scala.util.Success
import com.typesafe.config.ConfigFactory

object GreedySVBIExperiment extends App with Logging {
  val config = ConfigFactory.load()
  val settings = new Settings(config)
  
  def SEED_PERCENT_RANGE = Range(2, 32,  4)

  new File(settings.resultsDirectory).mkdirs()
  
  val allResults = new PrintWriter(new File(settings.resultsDirectory + "/all.res"))

  val cases = settings.testedCases

  val algorithms = settings.algorithms
  
  case class TestValue(ICvalue : Double, time : Double, greedySimilarity : Double, LTvalue : Double)
  
  case class TestResult(caseName: String, values : mutable.Map[(String, Int), TestValue]) {
    def this(name : String) = this(name, mutable.Map[(String, Int), TestValue]())
  }

  val results = ListBuffer[TestResult]()

  def performExperiment(data: ExperimentCase, seedSizes: Seq[Int]) 
      : Future[TestResult] = future {
    println("Data: " + data.name + " Seed sizes: " + seedSizes.mkString(","))
    val net = data.network
    val res = new TestResult(data.name)
    
    logger.info(s"Testing ${settings.referenceHeuristic._1} (${data.name})")
    val refResults = settings.referenceHeuristic._2(data.network)(seedSizes)
    data.network.clearCache()
    val refTimes = refResults.map(_._2)
    val refICQualities  = refResults.map(x => net.computeTotalInfluence(x._1))
    val refLTQualities = refResults.map(x => net.mcLinearThresholdSeedQuality(x._1, settings.MC_RUNS))
    
    val tvs = refTimes.zip(refICQualities).zip(refLTQualities).map(
        x => new TestValue(x._1._2, x._1._1.toMillis, 1.0, x._2))
    
    res.values ++= seedSizes.map((settings.referenceHeuristic._1, _)).zip(tvs)
    seedSizes.map((settings.referenceHeuristic._1, _)).zip(tvs).foreach{
      x => allResults.println(s"${x._1._1} ${x._1._2} ${x._2.ICvalue} ${x._2.time} " + 
          s"${x._2.greedySimilarity} ${x._2.LTvalue}")
    }
    allResults.flush()
     
    data.network.clearCache()
    refICQualities.zip(refTimes).foreach{
      x => logger.info(s"${settings.referenceHeuristic._1}(${data.name}): ${x._1} time: ${x._2}")
    }
    
    for (heuristic <- algorithms) {
      logger.info(s"Testing ${heuristic._1}(${data.name})")
      val results = heuristic._2(data.network)(seedSizes)
      data.network.clearCache()
      val times = results.map(_._2)
      val ICqualities = results.map(x => net.computeTotalInfluence(x._1))
      val LTqualities = results.map(x => net.mcLinearThresholdSeedQuality(x._1, settings.MC_RUNS))
      val similarities = results.map(_._1).zip(refResults.map(_._1)).map(
          x => setSimilarity(x._1.toSet, x._2.toSet))

      val tvs = ICqualities.zip(times).zip(similarities.zip(LTqualities)).map(
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
            logger.warn(e.getMessage)
            logger.warn(e.getStackTraceString)
    }
    future.map {
      case result =>
        logger.info(s"Printing results ${data.name}")
        val filename = data.name.replace(".", "_")
        printResultsToFile(s"${settings.resultsDirectory}/$filename.res",
          result)
        printResultsToFile(s"${settings.resultsDirectory}/${filename}_times.res",
          result, _.time)
        printResultsToFile(s"${settings.resultsDirectory}/${filename}_greedySimilarities.res",
          result, _.greedySimilarity)
        printResultsToFile(s"${settings.resultsDirectory}/${filename}_lt.res",
          result, _.LTvalue)
        logger.info(s"Results printed ${data.name}")
    }
  }
  
  Await.result(Future.sequence(futures), Duration.Inf)
  allResults.close()

  def printResultsToFile(filename : String, result : TestResult, 
      unpack : TestValue => Double = _.ICvalue) {
    val seedSizesSorted = result.values.keys.map(_._2).toList.sorted
    val writer = new PrintWriter(new File(filename))
    
    def aligningSpaces(s : String) = " " * (20 - s.size)
    def stringAligned(s : String) = s + aligningSpaces(s)
    writer.println(s"${stringAligned("seedSize")}\t" + seedSizesSorted.mkString("\t"))
    (settings.referenceHeuristic :: algorithms).foreach{
      heuristic =>
        val resultsSorted = seedSizesSorted.map(x => unpack(result.values((heuristic._1, x))))
          .mkString("\t")
        writer.println(stringAligned(heuristic._1) + "\t" + resultsSorted)
    }
    writer.close()
  }
}