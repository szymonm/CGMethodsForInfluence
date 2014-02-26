package pl.szymonmatejczyk.competetiveShapley

import collection._

trait LinearThreshold {
  self : InfluenceNetwork =>

  def activationThresholdGenerator() : Double = r.nextDouble
  
  def randomLinearThresholdSeedQuality(seed : Seq[Int],
      activationThresholdGenerator : (() => Double) = activationThresholdGenerator) : Int = {
    val activationThresholdsLeft = mutable.Map[g.NodeT, Double]() ++ 
      g.nodes.map((_, activationThresholdGenerator()))
    
    val active = mutable.Set[g.NodeT]()
    val lastPhaseActivated = mutable.Queue[g.NodeT]() ++ seed.map(g.get(_))
    
    while (lastPhaseActivated.nonEmpty) {
      val current = lastPhaseActivated.dequeue
      if (!active.contains(current)) {
        current.outgoing.filterNot(e => active.contains(e._2)).foreach {
          e =>
            activationThresholdsLeft += ((e._2, activationThresholdsLeft(e._2) - 
                e.weight / weightDenominator))
            if (activationThresholdsLeft(e._2) <= 0.0)
              lastPhaseActivated += e._2
        }
        active += current
      }
    }
    
    active.size
  }
  
  def mcLinearThresholdSeedQuality(seed : Seq[Int], runs : Int, 
      atg : (() => Double) = activationThresholdGenerator) : Double = {
    1.to(runs).par.map{_ => randomLinearThresholdSeedQuality(seed, atg).toDouble}.sum / runs
  }
}