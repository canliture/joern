package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode}
import io.shiftleft.semanticcpg.accesspath.AccessPath

type APReachingDefDomainType = Map[AccessPath, Set[StoredNode]]
val emptyApDomain = Map[AccessPath, Set[StoredNode]]()

object APReachingDefProblem {

  private def meetFunction(x: APReachingDefDomainType, y: APReachingDefDomainType) : APReachingDefDomainType = {
    (x.keySet ++ y.keySet).map(k =>
      k -> (x.getOrElse(k, Set.empty) ++ y.getOrElse(k, Set.empty))
    )
    .toMap
  }

  def create(method: Method): DataFlowProblem[StoredNode, APReachingDefDomainType] = {
    val flowGraph = new ReachingDefFlowGraph(method)
    val transfer  = new APReachingDefTransferFunction(flowGraph)
    val init      = new APReachingDefInOut()
    val meet      = meetFunction
    new DataFlowProblem[StoredNode, APReachingDefDomainType](flowGraph, transfer, meet, init, true, emptyApDomain)
  }
}

class APReachingDefTransferFunction(x: FlowGraph[StoredNode]) extends TransferFunction[StoredNode, APReachingDefDomainType] {
  override def apply(n: StoredNode, x: APReachingDefDomainType): APReachingDefDomainType = {
    ???
  }
}

class APReachingDefInOut extends InOutInit[StoredNode, APReachingDefDomainType] {
  override def initIn: Map[StoredNode, APReachingDefDomainType] =
    Map
      .empty[StoredNode, APReachingDefDomainType]
      .withDefaultValue(emptyApDomain)

  override def initOut: Map[StoredNode, APReachingDefDomainType] =
    Map
      .empty[StoredNode, APReachingDefDomainType]
      .withDefaultValue(emptyApDomain)
}