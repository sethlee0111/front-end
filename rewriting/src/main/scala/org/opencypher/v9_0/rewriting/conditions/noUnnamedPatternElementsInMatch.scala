/*
 * Copyright (c) Neo4j Sweden AB (http://neo4j.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.opencypher.v9_0.rewriting.conditions

import org.opencypher.v9_0.ast.Match
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.rewriting.ValidatingCondition

case object noUnnamedPatternElementsInMatch extends ValidatingCondition {

  def apply(that: Any): Seq[String] = {
    val into = collectNodesOfType[Match]().apply(that).map(_.pattern)
    into.flatMap(unnamedNodePatterns) ++ into.flatMap(unnamedRelationshipPatterns)
  }

  private def unnamedRelationshipPatterns(that: Any): Seq[String] = {
    collectNodesOfType[RelationshipPattern]().apply(that).collect {
      case rel @ RelationshipPattern(None, _, _, _, _, _, _) =>
        s"RelationshipPattern at ${rel.position} is unnamed"
    }
  }

  private def unnamedNodePatterns(that: Any): Seq[String] = {
    collectNodesOfType[NodePattern]().apply(that).collect {
      case node @ NodePattern(None, _, _, _) =>
        s"NodePattern at ${node.position} is unnamed"
    }
  }

  override def name: String = productPrefix
}
