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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.ast.AliasedReturnItem
import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.Match
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.SubqueryCall
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.rewriting.conditions.containsNoReturnAll
import org.opencypher.v9_0.rewriting.rewriters.factories.ASTRewriterFactory
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.helpers.fixedPoint
import org.opencypher.v9_0.util.inSequence
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.topDown

case object IndependentWithsMovedAfterMatch extends StepSequencer.Condition

/**
 * Rewrites `WITH 1 AS foo MATCH (x)` => `MATCH (x) WITH 1 AS foo, x`.
 *
 * This is beneficial for ordering optimizations of the planner which are harder to apply for all but the first QueryGraph.
 *
 * This could potentially move projections to a point of higher cardinality, but the cached properties mechanism
 * should take care that expensive projections are pushed down again.
 */
case object moveWithPastMatch extends Rewriter with StepSequencer.Step with ASTRewriterFactory {

  override def getRewriter(
    semanticState: SemanticState,
    parameterTypeMapping: Map[String, CypherType],
    cypherExceptionFactory: CypherExceptionFactory,
    anonymousVariableNameGenerator: AnonymousVariableNameGenerator
  ): Rewriter = instance

  override def apply(that: AnyRef): AnyRef = instance(that)

  override def preConditions: Set[StepSequencer.Condition] = Set(
    containsNoReturnAll // It's better to know the variables in WITH already
  )

  override def postConditions: Set[StepSequencer.Condition] = Set(IndependentWithsMovedAfterMatch)

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set(
    ProjectionClausesHaveSemanticInfo // It can invalidate this condition by copying WITH clauses
  )

  private val subqueryRewriter: Rewriter = topDown(Rewriter.lift {
    case s: SubqueryCall =>
      s.copy(part = s.part.endoRewrite(innerRewriter(insideSubquery = true)))(s.position)
  })

  private val instance: Rewriter = inSequence(innerRewriter(insideSubquery = false), subqueryRewriter)

  sealed private trait QuerySection {
    def clauses: Seq[Clause]
  }

  private case class MatchGroup(clauses: Seq[Match]) extends QuerySection {
    def containsOptionalMatch: Boolean = clauses.exists(_.optional)

    def usesVariableFromWith(w: With): Boolean = {
      // We can be sure all return items are aliased at this point
      clauses.exists {
        m => m.folder.findAllByClass[LogicalVariable].exists(w.returnItems.items.flatMap(_.alias).contains)
      }
    }

    def allExportedVariablesAsReturnItems: Seq[AliasedReturnItem] =
      clauses.flatMap(_.allExportedVariables.map(v => AliasedReturnItem(v))).distinct
  }

  private case class MovableWith(`with`: With) extends QuerySection {
    override def clauses: Seq[Clause] = Seq(`with`)
  }

  private case class OtherClause(clause: Clause) extends QuerySection {
    override def clauses: Seq[Clause] = Seq(clause)
  }

  private def innerRewriter(insideSubquery: Boolean): Rewriter = fixedPoint(topDown(
    Rewriter.lift {
      case q: SingleQuery =>
        // Partition the clauses into sections
        val sections = q.clauses.foldLeft(Seq.empty[QuerySection]) {
          case (previousSections :+ (mg @ MatchGroup(matches)), m: Match) if !mg.containsOptionalMatch || m.optional =>
            // Add MATCH to previous MatchGroup
            previousSections :+ MatchGroup(matches :+ m)
          case (previousSections, m: Match) =>
            // New MatchGroup
            previousSections :+ MatchGroup(Seq(m))
          case (previousSections, w: With)
            if isMovableWith(w) && previousSections.forall(_.isInstanceOf[MovableWith]) =>
            // A with clause that can potentially be moved. Only if at beginning or after other movable WITHs.
            previousSections :+ MovableWith(w)
          case (previousSections, clause) =>
            // New OtherClause
            previousSections :+ OtherClause(clause)
        }

        // Move WITHs around
        val newSections = sections.foldLeft(Seq.empty[QuerySection]) {
          case (previousSections :+ MovableWith(w), mg: MatchGroup)
            if !(insideSubquery && q.importWith.contains(w)) &&
              !mg.usesVariableFromWith(w) =>
            // The WITH can be moved past the MatchGroup
            val newWith = w.copy(returnItems = w.returnItems.copy(items =
              w.returnItems.items ++ mg.allExportedVariablesAsReturnItems)(w.returnItems.position))(w.position)

            previousSections :+ mg :+ MovableWith(newWith)
          case (previousSections, section) =>
            previousSections :+ section
        }

        // Extract individual clauses again
        val newClauses = newSections.flatMap(_.clauses)
        q.copy(clauses = newClauses)(q.position)
    },
    stopper = _.isInstanceOf[SubqueryCall]
  ))

  private def isMovableWith(w: With): Boolean = {
    w.skip.isEmpty &&
    w.limit.isEmpty &&
    w.orderBy.isEmpty &&
    w.where.isEmpty &&
    !w.returnItems.includeExisting &&
    !w.returnItems.containsAggregate &&
    !w.distinct
  }
}
