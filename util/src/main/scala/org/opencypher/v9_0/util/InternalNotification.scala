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
package org.opencypher.v9_0.util

/**
 * Describes a notification
 */
trait InternalNotification

case class CartesianProductNotification(position: InputPosition, isolatedVariables: Set[String])
    extends InternalNotification

case class UnboundedShortestPathNotification(position: InputPosition) extends InternalNotification

case class DeprecatedFunctionNotification(position: InputPosition, oldName: String, newName: String)
    extends InternalNotification

case class DeprecatedVarLengthBindingNotification(position: InputPosition, variable: String)
    extends InternalNotification

case class DeprecatedRepeatedRelVarInPatternExpression(position: InputPosition, relName: String)
    extends InternalNotification

case class DeprecatedCoercionOfListToBoolean(position: InputPosition) extends InternalNotification

case class DeprecatedSelfReferenceToVariableInCreatePattern(position: InputPosition) extends InternalNotification

case class SubqueryVariableShadowing(position: InputPosition, varName: String) extends InternalNotification

case class DeprecatedAmbiguousGroupingNotification(pos: InputPosition, hint: Option[String])
    extends InternalNotification
