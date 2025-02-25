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

import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.symbols.CTString
import org.opencypher.v9_0.util.symbols.CypherType

case class ShowColumn(variable: LogicalVariable, cypherType: CypherType, name: String)

object ShowColumn {

  def apply(name: String, cypherType: CypherType = CTString)(position: InputPosition): ShowColumn =
    ShowColumn(Variable(name)(position), cypherType, name)
}

case class DefaultOrAllShowColumns(useAllColumns: Boolean, columns: List[ShowColumn])

object DefaultOrAllShowColumns {

  def apply(useAllColumns: Boolean, brief: List[ShowColumn], all: List[ShowColumn]): DefaultOrAllShowColumns = {
    if (useAllColumns) DefaultOrAllShowColumns(useAllColumns, all) else DefaultOrAllShowColumns(useAllColumns, brief)
  }
}
