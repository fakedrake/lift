package rewriting.MapTree
import ir.ast._
import rewriting.utils.NumberExpression

import scala.collection.mutable.ArrayBuffer


case class ControlMapNum(
                          level1:Boolean,
                          level2:Boolean,
                          level3:Boolean,
                          level4:Boolean
                        )

class MapTree{
  val NodesMap = collection.mutable.Map[MapTreeNode,Int]()
  val RootNode = new MapTreeNode
  var MaxAvaDepth:Int = 1000000
  var MaxDepth:Int = 0
  def apply(l:Lambda)={
    parse(l)
  }
  def parse(l:Lambda):Unit ={
    val numbering = NumberExpression.breadthFirst(l)
    build(RootNode,l.body,1)(numbering)
  }

  private def build(parentNode:MapTreeNode,expr:Expr,depth:Int)(implicit numbering:collection.Map[Expr,Int]):Unit =  {
    expr match{
      case call:FunCall =>

        call.f match{

          case lambda:Lambda =>
            build(parentNode,lambda.body,depth)

          case map:ir.ast.Map =>
            val currNode = new MapTreeNode
            currNode.Depth = depth
            if(MaxDepth < depth)
              MaxDepth = depth

            build(currNode,map.f.body,depth+1)

            parentNode.Child += currNode
            NodesMap += (currNode -> numbering(expr))

          case fp:FPattern => build(parentNode,fp.f.body,depth)

          case uf:UserFun =>
            parentNode.shouldSequential = true
            if(MaxAvaDepth >= depth)
              MaxAvaDepth = depth -1

          case _=>
        }
        call.args.foreach(build(parentNode,_,depth))
      case _=>
    }
  }

  def getMapsIdOnLevel(level:Int):Array[Int] = {
    NodesMap.filterKeys(_.Depth == level).values.toArray[Int]
  }
  def getMapsNodeOnLevel(level:Int):Array[MapTreeNode] = {
    NodesMap.filterKeys(_.Depth == level).keys.toArray[MapTreeNode]
  }

  /*def build(parentNode: MapTreeNode,funDecl:FunDecl,depth:Int):Unit = {

      funDecl match{
        case lambda:Lambda => build(parentNode,lambda.body,depth)
        case map:ir.ast.Map =>
          val currNode = new MapTreeNode
          currNode.Depth = depth
          build(currNode,map.f,depth+1)
          parentNode.Child += currNode

        case fp:FPattern => build(parentNode,fp.f,depth)
        case uf:UserFun => parentNode.shouldConsequence = true

      }
    }*/
}
class MapTreeNode{
  var Depth:Int = 0
  val Child = collection.mutable.ArrayBuffer[MapTreeNode]()
  var shouldSequential = false

}



object findAllMapsLowering{
  def findAllLoweringBySwitch(mapTree: MapTree,totalLevels:Int):Unit = {
    val answerSet = scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
    var remainSet = scala.collection.immutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()

    var initSet = scala.collection.immutable.Map[MapTreeNode,Int]()
    //get init input
    mapTree.NodesMap.keys.foreach(
        currNode => {
          if (currNode.Depth <= totalLevels && currNode.Depth != 0) {
            initSet += (currNode -> currNode.Depth)
          }
          if (currNode.Depth > totalLevels) {
            initSet += (currNode -> -1)
          }
        }
    )
    remainSet += initSet

    while(!remainSet.isEmpty){
      var nextToDeal = scala.collection.immutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
      remainSet.foreach(
        currState => {
          nextToDeal ++= trySwitch(mapTree,currState)
        }
      )
      answerSet ++= remainSet
      remainSet = nextToDeal
    }
  }

  private def trySwitch(mapTree: MapTree,currState:scala.collection.immutable.Map[MapTreeNode,Int]):scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]  ={
    val nextToDeal = scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
    currState.foreach(
      currNode =>{
        val currTreeNode:MapTreeNode = currNode._1
        val currMaping:Int = currNode._2
        if(currMaping != -1 && (!currTreeNode.shouldSequential) && currTreeNode.Child.forall(currState(_) == -1)){
          //nextToDeal += (currState - currTreeNode -- currTreeNode.Child + (currTreeNode -> -1) + (currTreeNode.Child.map()))
          nextToDeal += currState.map(x => {
            if (x._1 == currTreeNode) {
              (currTreeNode -> -1)
            }
            else{
              if(currTreeNode.Child.contains(x._1)){
                (x._1 -> currMaping)
              }
              else{
                x
              }
            }
          })
        }
      }
    )
    nextToDeal

  }

  def findAllLoweringByEnum(mapTree: MapTree,totalLevel:Int,controlMapNum: ControlMapNum):ArrayBuffer[scala.collection.immutable.Map[Int,Int]] ={

    lowerNext(mapTree.RootNode,0,totalLevel)(controlMapNum,mapTree.NodesMap)

  }

  private def lowerNext(currNode:MapTreeNode,currLevel:Int,totalLevel:Int)(implicit controlMapNum: ControlMapNum,nodesMap:collection.mutable.Map[MapTreeNode,Int]):ArrayBuffer[scala.collection.immutable.Map[Int,Int]]={
    val res = ArrayBuffer[scala.collection.immutable.Map[Int,Int]]()

    //currNode should be sequential, but we can't reach the lowest level, so back trace
    if(currNode.shouldSequential && currLevel < totalLevel -1)
      return res

    //currNode doesn't have child, but we can't reach the lowest level, so back trace
    if(currNode.Child.length == 0 && currLevel < totalLevel -1){
      return res
    }

    //currNode does't have child, so we finally reach an end
    if(currNode.Child.length == 0){

      //should not lower this level
      if(currLevel == totalLevel){
        return ArrayBuffer[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](nodesMap(currNode) -> 0))
      }

      //should lower this level
      if(currLevel == totalLevel -1){
        return ArrayBuffer[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](nodesMap(currNode) -> totalLevel))
      }
      assert(false)
    }

    //lower this level
    if(currLevel < totalLevel){

      currNode.Child.foreach((childNode) =>{
        val childResult = lowerNext(childNode,currLevel+1,totalLevel)
        childResult.foreach((oneResult) =>{
          res += (oneResult + (nodesMap(currNode) -> currLevel))
        })
      })

    }



    //don't lower this level
    currNode.Child.foreach((childNode) =>{
      val childResult = lowerNext(childNode,currLevel,totalLevel)
      childResult.foreach((oneResult) =>{
        res += (oneResult + (nodesMap(currNode) -> 0 ))
      })
    })

    res



  }
}

