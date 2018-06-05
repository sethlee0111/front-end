/*
 * Copyright © 2002-2018 Neo4j Sweden AB (http://neo4j.com)
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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.util.{InputPosition, InternalException, Rewriter, topDown}
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.{PropertyKeyName, Variable}

/*
Handles rewriting map projection elements to literal entries when possible. If the user
has used an all properties selector ( n{ .* } ), we need to do the work in runtime.
In these situations, the rewriter turns as much as possible into literal entries,
so the runtime only has two cases to handle - literal entries and the special all-props selector.

We can't rewrite all the way to literal maps, since map projections yield a null map when the map_variable is null,
and the same behaviour can't be mimicked with literal maps.
 */
case class desugarMapProjection(state: SemanticState) extends Rewriter {
  def apply(that: AnyRef): AnyRef = topDown(instance).apply(that)

  private val instance: Rewriter = Rewriter.lift {
    case e@MapProjection(id, items, definitionPos) =>

      def propertySelect(propertyPosition: InputPosition, name: String): LiteralEntry = {
        val key = PropertyKeyName(name)(propertyPosition)
        val idPos = definitionPos.getOrElse(throw new InternalException("MapProjection definition pos is not known"))
        val newIdentifier = Variable(id.name)(idPos)
        val value = Property(newIdentifier, key)(propertyPosition)
        LiteralEntry(key, value)(propertyPosition)
      }

      def identifierSelect(id: Variable): LiteralEntry =
        LiteralEntry(PropertyKeyName(id.name)(id.position), id)(id.position)

      var includeAllProps = false

      val mapExpressionItems = items.flatMap {
        case x: LiteralEntry => Some(x)
        case x: AllPropertiesSelector => includeAllProps = true; None
        case PropertySelector(property: Variable) => Some(propertySelect(property.position, property.name))
        case VariableSelector(identifier: Variable) => Some(identifierSelect(identifier))
      }

      DesugaredMapProjection(id, mapExpressionItems, includeAllProps)(e.position)
  }
}
