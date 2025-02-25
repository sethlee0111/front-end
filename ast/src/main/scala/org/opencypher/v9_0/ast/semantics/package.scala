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

import org.opencypher.v9_0.util.symbols.TypeSpec

import scala.language.implicitConversions

package object semantics {

  type SemanticCheck = SemanticState => SemanticCheckResult
  type TypeGenerator = SemanticState => TypeSpec

  // Allows joining of two (SemanticState => SemanticCheckResult) funcs together (using then)
  implicit def chainableSemanticCheck(check: SemanticCheck): ChainableSemanticCheck = new ChainableSemanticCheck(check)

  // Allows joining of a (SemanticState => SemanticCheckResult) func to a (SemanticState => Either[SemanticErrorDef, SemanticState]) func
  implicit def chainableSemanticEitherFunc(func: SemanticState => Either[SemanticErrorDef, SemanticState])
    : ChainableSemanticCheck = new ChainableSemanticCheck(func)

  // Allows joining of a (SemanticState => SemanticCheckResult) func to a (SemanticState => Seq[SemanticErrorDef]) func
  implicit def chainableSemanticErrorDefsFunc(func: SemanticState => Seq[SemanticErrorDef]): ChainableSemanticCheck =
    new ChainableSemanticCheck(func)

  // Allows joining of a (SemanticState => SemanticCheckResult) func to a (SemanticState => Option[SemanticErrorDef]) func
  implicit def chainableSemanticOptionFunc(func: SemanticState => Option[SemanticErrorDef]): ChainableSemanticCheck =
    new ChainableSemanticCheck(func)

  // Allows using a (SemanticState => Either[SemanticErrorDef, SemanticState]) func, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticEitherFunc(func: SemanticState => Either[SemanticErrorDef, SemanticState]): SemanticCheck =
    state => {
      func(state).fold(error => SemanticCheckResult.error(state, error), s => SemanticCheckResult.success(s))
    }

  // Allows using a (SemanticState => Seq[SemanticErrorDef]) func, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticErrorDefsFunc(func: SemanticState => Seq[SemanticErrorDef]): SemanticCheck = state => {
    SemanticCheckResult(state, func(state))
  }

  // Allows using a (SemanticState => Option[SemanticErrorDef]) func, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticErrorDefFunc(func: SemanticState => Option[SemanticErrorDef]): SemanticCheck = state => {
    SemanticCheckResult.error(state, func(state))
  }

  // Allows using a sequence of SemanticErrorDef, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticErrorDefs(errors: Seq[SemanticErrorDef]): SemanticCheck = SemanticCheckResult(_, errors)

  // Allows using a single SemanticErrorDef, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticErrorDef(error: SemanticErrorDef): SemanticCheck = SemanticCheckResult.error(_, error)

  // Allows using an optional SemanticErrorDef, where a (SemanticState => SemanticCheckResult) func is expected
  implicit def liftSemanticErrorDefOption(error: Option[SemanticErrorDef]): SemanticCheck =
    SemanticCheckResult.error(_, error)

  // Allows starting with a sequence of SemanticErrorDef, and joining to a (SemanticState => SemanticCheckResult) func (using then)
  implicit def liftSemanticErrorDefsAndChain(errors: Seq[SemanticErrorDef]): ChainableSemanticCheck =
    liftSemanticErrorDefs(errors)

  // Allows starting with a single SemanticErrorDef, and joining to a (SemanticState => SemanticCheckResult) func (using then)
  implicit def liftSemanticErrorDefAndChain(error: SemanticErrorDef): ChainableSemanticCheck =
    liftSemanticErrorDef(error)

  // Allows starting with an optional SemanticErrorDef, and joining to a (SemanticState => SemanticCheckResult) func (using then)
  implicit def liftSemanticErrorDefOptionAndChain(error: Option[SemanticErrorDef]): ChainableSemanticCheck =
    liftSemanticErrorDefOption(error)

  // Allows folding a semantic checking func over a collection
  implicit def optionSemanticChecking[A](option: Option[A]): OptionSemanticChecking[A] =
    new OptionSemanticChecking(option)

  implicit def traversableOnceSemanticChecking[A](traversable: IterableOnce[A]): TraversableOnceSemanticChecking[A] =
    new TraversableOnceSemanticChecking(traversable)

  // Allows calling semanticCheck on an optional SemanticCheckable object
  implicit def semanticCheckableOption[A <: SemanticCheckable](option: Option[A]): SemanticCheckableOption[A] =
    new SemanticCheckableOption(option)

  // Allows calling semanticCheck on a traversable sequence of SemanticCheckable objects
  implicit def semanticCheckableTraversableOnce[A <: SemanticCheckable](traversable: IterableOnce[A])
    : SemanticCheckableTraversableOnce[A] = new SemanticCheckableTraversableOnce(traversable)

  implicit final class RichSemanticCheck(val check: SemanticCheck) extends AnyVal {

    // Only run a check if a given feature is enabled
    def ifFeatureEnabled(feature: SemanticFeature): SemanticCheck =
      (s: SemanticState) => if (s.features(feature)) check(s) else SemanticCheckResult.success(s)

    // Only run a check if a given feature is *not* enabled
    def unlessFeatureEnabled(feature: SemanticFeature): SemanticCheck =
      (s: SemanticState) => if (!s.features(feature)) check(s) else SemanticCheckResult.success(s)
  }
}
