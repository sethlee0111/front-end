/*
 * Copyright © 2002-2020 Neo4j Sweden AB (http://neo4j.com)
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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.expressions.Parameter
import org.opencypher.v9_0.expressions.SensitiveParameter
import org.opencypher.v9_0.expressions.SensitiveString
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.METADATA_COLLECTION
import org.opencypher.v9_0.util.Foldable.SkipChildren
import org.opencypher.v9_0.util.ObfuscationMetadata
import org.opencypher.v9_0.util.StepSequencer

object ObfuscationMetadataCollection extends Phase[BaseContext, BaseState, BaseState] {

  override def phase: CompilationPhaseTracer.CompilationPhase = METADATA_COLLECTION

  override def description: String = "collect sensitive literals and parameters"

  override def postConditions: Set[StepSequencer.Condition] = Set.empty

  override def process(from: BaseState, context: BaseContext): BaseState = {
    val extractedParamNames = from.maybeExtractedParams.map(_.keys.toSet).getOrElse(Set.empty)
    val preParserOffset = from.startPosition.map(_.offset).getOrElse(0)
    val parameters = from.statement().findByAllClass[Parameter]

    val offsets = collectSensitiveLiteralOffsets(from.statement(), extractedParamNames, preParserOffset)
    val sensitiveParams = collectSensitiveParameterNames(parameters, extractedParamNames)

    from.withObfuscationMetadata(ObfuscationMetadata(offsets, sensitiveParams))
  }

  private def collectSensitiveLiteralOffsets(statement: Statement, extractedParamNames: Set[String], preParserOffset: Int): Vector[Int] =
    statement.treeFold(Vector.empty[Int]) {
      case literal: SensitiveString =>
        acc => SkipChildren(acc :+ literal.position.offset)
      case parameter: SensitiveParameter if extractedParamNames.contains(parameter.name) =>
        acc => SkipChildren(acc :+ parameter.position.offset)
    }.distinct.sorted.map(_ + preParserOffset)

  private def collectSensitiveParameterNames(queryParams: Seq[Parameter], extractedParamNames: Set[String]): Set[String] =
    queryParams.findByAllClass[SensitiveParameter].map(_.name).toSet -- extractedParamNames
}
