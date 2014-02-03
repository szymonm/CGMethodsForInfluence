object test1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import scala.collection.generic.CanBuildFrom
  import scala.collection._
  import scalax.collection.Graph
  import scalax.collection.GraphPredef._
  import scalax.collection.GraphEdge._
  import scalax.collection.edge.Implicits._
  import scala.util.Random
  val r = new Random                              //> r  : scala.util.Random = scala.util.Random@48abe515
  val g = Graph(1, 2, 3, 1~2, 2~3)                //> g  : scalax.collection.Graph[Int,scalax.collection.GraphEdge.UnDiEdge] = Gra
                                                  //| ph(1, 2, 3, 1~2, 2~3)
  
  g.getClass().getSimpleName()                    //> res0: String = DefaultGraphImpl
  
  
  
}