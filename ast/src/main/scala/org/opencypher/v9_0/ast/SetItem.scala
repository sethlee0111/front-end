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
package org.opencypher.v9_0.ast

import org.opencypher.v9_0.ast.semantics.SemanticAnalysisTooling
import org.opencypher.v9_0.ast.semantics.SemanticCheck
import org.opencypher.v9_0.ast.semantics.SemanticCheckResult
import org.opencypher.v9_0.ast.semantics.SemanticCheckable
import org.opencypher.v9_0.ast.semantics.SemanticError
import org.opencypher.v9_0.ast.semantics.SemanticExpressionCheck
import org.opencypher.v9_0.ast.semantics.SemanticPatternCheck
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.ExistsSubClause
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LabelName
import org.opencypher.v9_0.expressions.LogicalProperty
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTMap
import org.opencypher.v9_0.util.symbols.CTNode
import org.opencypher.v9_0.util.symbols.CTRelationship

sealed trait SetItem extends ASTNode with SemanticCheckable

case class SetLabelItem(variable: Variable, labels: Seq[LabelName])(val position: InputPosition) extends SetItem {

  def semanticCheck =
    SemanticExpressionCheck.simple(variable) chain
      SemanticPatternCheck.checkValidLabels(labels, position) chain
      SemanticExpressionCheck.expectType(CTNode.covariant, variable)
}

sealed trait SetProperty extends SetItem with SemanticAnalysisTooling

case class SetPropertyItem(property: LogicalProperty, expression: Expression)(val position: InputPosition)
    extends SetProperty {

  def semanticCheck =
    checkForExists chain
      SemanticExpressionCheck.simple(property) chain
      SemanticPatternCheck.checkValidPropertyKeyNames(Seq(property.propertyKey), property.position) chain
      SemanticExpressionCheck.simple(expression) chain
      expectType(CTNode.covariant | CTRelationship.covariant, property.map)

  private def checkForExists: SemanticCheck = {
    val invalid: Option[Expression] = expression.folder.treeFind[Expression] { case _: ExistsSubClause => true }
    invalid.map(exp => SemanticError("The EXISTS subclause is not valid inside a SET clause.", exp.position))
  }
}

case class SetPropertyItems(map: Expression, items: Seq[(PropertyKeyName, Expression)])(val position: InputPosition)
    extends SetProperty {

  def semanticCheck = {

    val properties = items.map(_._1)
    val expressions = items.map(_._2)
    checkForExists chain
      SemanticExpressionCheck.simple(map) chain
      semanticCheckFold(properties) { property =>
        SemanticPatternCheck.checkValidPropertyKeyNames(Seq(property), property.position)
      } chain
      SemanticExpressionCheck.simple(expressions) chain
      expectType(CTNode.covariant | CTRelationship.covariant, map)
  }

  private def checkForExists: SemanticCheck = (state: SemanticState) => {
    val invalid = items.map(_._2).flatMap(e => e.folder.treeFind[Expression] { case _: ExistsSubClause => true })
    val errors: Seq[SemanticError] =
      invalid.map(exp => SemanticError("The EXISTS subclause is not valid inside a SET clause.", exp.position))
    SemanticCheckResult(state, errors)
  }
}

case class SetExactPropertiesFromMapItem(variable: Variable, expression: Expression)(val position: InputPosition)
    extends SetProperty {

  def semanticCheck =
    SemanticExpressionCheck.simple(variable) chain
      expectType(CTNode.covariant | CTRelationship.covariant, variable) chain
      SemanticExpressionCheck.simple(expression) chain
      expectType(CTMap.covariant, expression)
}

case class SetIncludingPropertiesFromMapItem(variable: Variable, expression: Expression)(val position: InputPosition)
    extends SetProperty {

  def semanticCheck =
    SemanticExpressionCheck.simple(variable) chain
      expectType(CTNode.covariant | CTRelationship.covariant, variable) chain
      SemanticExpressionCheck.simple(expression) chain
      expectType(CTMap.covariant, expression)
}
