package pl.szymonmatejczyk.competetiveShapley

import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader.FileType
import pl.szymonmatejczyk.competetiveShapley.randomGraphs.GraphGenerator

package object experiments {
  case class GraphSizeRestriction(maxSize : Int)

  sealed class ExperimentCase(val name: String, val network: InfluenceNetwork)

  case class DataCase(override val name: String, file: String, filetype: FileType,
                 withWeights: Boolean = false)(implicit restriction: GraphSizeRestriction)
    extends ExperimentCase(name, InfluenceNetwork.fromFile(file, filetype, withWeights).
      restrictSize(restriction.maxSize))

  case class GeneratedCase(generator: GraphGenerator, size: Int)
    extends ExperimentCase(generator.getClass.getSimpleName,
      new InfluenceNetwork(generator.generateGraph[Int](1 to size)))
}
