package org.opencypher.v9_0.frontend.join_graph_analytics

import org.apache.commons.lang3.mutable.MutableInt
import org.opencypher.v9_0.ast.{Match, Query, SingleQuery, Statement}
import org.opencypher.v9_0.ast.factory.neo4j.JavaCCParser
import org.opencypher.v9_0.expressions.{LabelName, NodePattern, RelTypeName, RelationshipChain, RelationshipPattern, Variable}
import org.opencypher.v9_0.frontend.join_graph_analytics
import org.opencypher.v9_0.frontend.phases.Namespacer
import org.opencypher.v9_0.util.{ASTNode, AnonymousVariableNameGenerator, OpenCypherExceptionFactory, Ref}
import org.opencypher.v9_0.util.Foldable.TreeAny

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object JoinGraph {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val query = args(0)
      println(getPrettyPrintGraphFromQuery(query))
    }
  }

  def getPrettyPrintGraphFromQuery(queryText : String): String = {
    val ast = parse(queryText)
    val lst = ListBuffer.empty[join_graph_analytics.RelationEdge]
    val vv_num : MutableInt = new MutableInt(0)
    for (mch <- getMatchList(ast)) {
      getRelationEdges(mch.treeChildren, lst, vv_num)
    }
    println(lst)
    val hpgrph = getHypergraphFromRelationEdges(lst)
    println(hpgrph)
    prettyPrintGraph(hpgrph)
  }

  def prettyPrintGraph(graph: mutable.HashMap[String, ListBuffer[String]]): String = {
    var res = new String("")
    graph.foreach{
      case (e, vList) => {
        if (res.nonEmpty) res += ",\n"
        res += e + " ("
        vList.zipWithIndex.foreach {
          case (v, idx) => {
            res += v
//            if (idx != vList.length - 1) res += ", "  // uncomment for newdetkdecomp format
            if (idx != vList.length - 1) res += " "
          }
        }
        res += ")"
      }
    }
    res += "."
    res
  }

  //// my functions
  // @TODO fix this so that it can be used with other clauses
  def getMatchList(ast: Statement): ListBuffer[Match] = {
    val res = ListBuffer.empty[Match]
    val children = ast.treeChildren.next().treeChildren.next().treeChildren
    while (children.hasNext) {
      val child = children.next()
      child match {
        case mch : Match => res += mch
        case _ => {}
      }
    }
    res
  }

  def getMatchList(ast: ASTNode, mList: ListBuffer[Match]): Unit = {
    ast match {
      case mch: Match => mList.addOne(mch)
      case _ => {
        val children = ast.treeChildren
        while (children.hasNext) getMatchList(children.next.asInstanceOf[ASTNode], mList)
      }
    }
  }

  def getRelationshipChainsFromMatch(mch : Match) : ListBuffer[RelationshipChain] = {
    var res = ListBuffer.empty[RelationshipChain]
    for (everyPath <- mch.pattern.patternParts)
      res += everyPath.element.asInstanceOf[RelationshipChain]
    res
  }

  def getRelationEdges(it : Iterator[AnyRef], res: ListBuffer[RelationEdge], vv_num: MutableInt): VariableVertex = {
    var vv = new VariableVertex("", "", -1)
    while (it.hasNext) {
      val node = it.next
      node match {
        case chain: RelationshipChain =>
          vv = getRelationEdgesFromRelationshipChain(chain, res, vv_num) // @WARNING cur_vv_num might overlap
        case _ =>
          vv = getRelationEdges(node.treeChildren, res, vv_num)
      }
    }
    vv
  }

  def getRelationEdgesFromRelationshipChain(rc: RelationshipChain, res: ListBuffer[RelationEdge], cur_vv_num: MutableInt): VariableVertex = {
    // assumes that RelationshipChain is always between two labels
    // last: last variable vertex read, which will be connected to new relationshipPattern (relation)
    val children = rc.treeChildren
    var vv = new VariableVertex("", "", -1)
    var re = new RelationEdge("")
    var cur_vv: MutableInt = cur_vv_num
    while (children.hasNext) {
      val node = children.next()
      node match {
        case chain: RelationshipChain =>
          vv = getRelationEdgesFromRelationshipChain(chain, res, cur_vv)
        case nodePattern: NodePattern =>
          vv = getVariableVertexFromNodePattern(nodePattern, cur_vv)
          cur_vv.increment()
          if (re.relationName.nonEmpty) {
            re.vertices.addOne(vv)
            res.addOne(re)
          }
        case relPattern : RelationshipPattern =>
          re = getRelationEdgeFromRelationshipPattern(relPattern)
          res.foreach( ree => {
            if (ree.relationName.equals(re.relationName)) re = ree
          })
          if (vv.unique_id != -1 && !re.vertices.contains(vv)) re.vertices.addOne(vv)
      }
    }
    vv
  }

  def getHypergraphFromRelationEdges(lst: ListBuffer[RelationEdge]): mutable.HashMap[String, ListBuffer[String]] = {
    val vertexMap = new mutable.HashMap[String, String]()
    val vertexWithNoVariableNameMap = new mutable.HashMap[String, String]()
    val graph = new mutable.HashMap[String, ListBuffer[String]]()
    //  identify all unique variables. Variables with only label name is identified as unique vertices
    //  as they do not need to be joined with others
    var num_vertex = 1
    var num_edge = 1
    lst.foreach(re => {
      var edgeName : String = "E" + new String(num_edge.toString)
      num_edge += 1
      graph += (edgeName -> new ListBuffer[String])
      re.vertices.foreach(vv => {
        var vertexName : String = "V"
        if (vv.variableName.nonEmpty) {
          if (vertexMap.contains(vv.variableName)) vertexName = vertexMap(vv.variableName)
          else {
            vertexName = "V" + new String(num_vertex.toString)
            num_vertex += 1
            vertexMap += (vv.variableName -> vertexName)
          }
        }
        else { // no variable name.
          if (vertexWithNoVariableNameMap.contains(vv.toString)) vertexName = vertexWithNoVariableNameMap(vv.toString)
          else {
            vertexName = "V" + new String(num_vertex.toString)
            num_vertex += 1
            vertexWithNoVariableNameMap += (vv.toString -> vertexName)
          }
        }
        graph(edgeName).addOne(vertexName)
      })
    })
    graph
  }

  def getVariableVertexFromNodePattern(np : NodePattern, id : MutableInt): VariableVertex = {
    // @WARNING: This assumes only a specific construction of NodePattern
    // @TODO: take account of the case where either one of variablename or labelname does not exist
    val nodeChildren = np.treeChildren
    var variableName : String = ""
    var labelName : String = ""
    try {
      variableName = nodeChildren.next().treeChildren.next().asInstanceOf[Variable].name
    } catch {
      // no variable name
      case _ => {}
    }
    try {
      labelName = nodeChildren.next().treeChildren.next().treeChildren.next().asInstanceOf[LabelName].name
    } catch {
      // no label name
      case _ => {}
    }
    new VariableVertex(_variableName = variableName, _labelName = labelName, _id = id.intValue())
  }

  def getRelationEdgeFromRelationshipPattern(rp: RelationshipPattern): RelationEdge = {
    val relTypeName = rp.treeChildren.drop(1).next().treeChildren.next()
    new RelationEdge(relTypeName.asInstanceOf[RelTypeName].name)
  }

  def constructJoinTree(queryText: String): String = {
    val statement = JavaCCParser.parse(queryText, OpenCypherExceptionFactory(None), new AnonymousVariableNameGenerator)
    val statementRewrite = statement.endoRewrite(Namespacer.projectUnions)
    val matchStatements = getMatchStatements(statementRewrite)
    for (st <- matchStatements)
      println(st)
    ""
  }

  def getMatchStatements(st: Statement): ListBuffer[Match] = {
    // @TODO: currently, this only works when Match clauses are wrapped with Query, SingleQuery, and List
    val res = ListBuffer.empty[Match]
    val matchList = st.returnColumns
    val children = st.treeChildren
    while (children.hasNext) {
      val child = children.next()
      println(child)
      if (child.isInstanceOf[SingleQuery]) {
        println("in")
      }
    }
    res
  }

  /////////

  def parse(queryText: String): Statement = {
    val statement = JavaCCParser.parse(queryText, OpenCypherExceptionFactory(None), new AnonymousVariableNameGenerator)
    // We have to project unions to materialize the UnionMappings so that we can find the Variables in them.
    statement.endoRewrite(Namespacer.projectUnions)
  }
}

class RelationEdge(_relationName: String) {
  def relationName : String = _relationName
  var vertices : ListBuffer[VariableVertex] = ListBuffer.empty[VariableVertex]
  override def toString = {
    var res : String = "[[RelationEdge] " + relationName
    vertices.foreach(res += " | " + _.toString)
    res += "]"
    res
  }
}

class VariableVertex(_variableName: String, _labelName: String, _id: Int) {
  def variableName : String = _variableName
  def labelName : String= _labelName
  def unique_id : Int = _id
  override def toString = {
    //    "[VariableVertex] variable: " + variableName + ", label: " + labelName
    if (variableName.nonEmpty) variableName
    else "L" + labelName + "#" + unique_id.toString
  }
  def empty : Boolean = {
    variableName.isEmpty && labelName.isEmpty
  }

  override def equals(obj: Any): Boolean = {
    obj.toString.equals(this.toString)
  }
}
