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
package org.opencypher.v9_0.ast.prettifier

import org.opencypher.v9_0.ast.AstConstructionTestSupport
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.SemanticDirection.BOTH
import org.opencypher.v9_0.expressions.SemanticDirection.INCOMING
import org.opencypher.v9_0.expressions.SemanticDirection.OUTGOING
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite
import org.opencypher.v9_0.util.test_helpers.TestName

class PatternStringifierTest extends CypherFunSuite with TestName with AstConstructionTestSupport {

  private val expressionStringifier = ExpressionStringifier(preferSingleQuotes = true)
  private val patternStringifier = PatternStringifier(expressionStringifier)

  test("(n:Foo:Bar {prop: 'test'} WHERE r.otherProp > 123)") {
    val pattern = NodePattern(
      Some(varFor("n")),
      Some(labelColonConjunction(
        labelAtom("Foo"),
        labelAtom("Bar")
      )),
      Some(mapOf("prop" -> literalString("test"))),
      Some(greaterThan(prop("r", "otherProp"), literalInt(123)))
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("({prop: 'test'})") {
    val pattern = NodePattern(
      None,
      None,
      Some(mapOf("prop" -> literalString("test"))),
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  // A bit extreme but ensures that the space before WHERE is only added when necessary
  test("(WHERE false)") {
    val pattern = NodePattern(
      None,
      None,
      None,
      Some(falseLiteral)
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("()") {
    val pattern = NodePattern(
      None,
      None,
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:A&B)") {
    val pattern = NodePattern(
      None,
      Some(
        labelConjunction(
          labelAtom("A"),
          labelAtom("B")
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:!A&B)") {
    val pattern = NodePattern(
      None,
      Some(
        labelConjunction(
          labelNegation(labelAtom("A")),
          labelAtom("B")
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:!(A&B))") {
    val pattern = NodePattern(
      None,
      Some(
        labelNegation(
          labelConjunction(
            labelAtom("A"),
            labelAtom("B")
          )
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:(A|A)&B)") {
    val pattern = NodePattern(
      None,
      Some(
        labelConjunction(
          labelDisjunction(
            labelAtom("A"),
            labelAtom("A")
          ),
          labelAtom("B")
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:!!(A&B)|C&B)") {
    val pattern = NodePattern(
      None,
      Some(
        labelDisjunction(
          labelNegation(
            labelNegation(
              labelConjunction(labelAtom("A"), labelAtom("B"))
            )
          ),
          labelConjunction(
            labelAtom("C"),
            labelAtom("B")
          )
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("(:!!((A&B|C)&B))") {
    val pattern = NodePattern(
      None,
      Some(
        labelNegation(
          labelNegation(
            labelConjunction(
              labelDisjunction(
                labelConjunction(labelAtom("A"), labelAtom("B")),
                labelAtom("C")
              ),
              labelAtom("B")
            )
          )
        )
      ),
      None,
      None
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("-[r:Foo|Bar*1..5 {prop: 'test'} WHERE r.otherProp > 123]->") {
    val pattern = RelationshipPattern(
      Some(varFor("r")),
      Seq(relTypeName("Foo"), relTypeName("Bar")),
      Some(Some(range(Some(1), Some(5)))),
      Some(mapOf("prop" -> literalString("test"))),
      Some(greaterThan(prop("r", "otherProp"), literalInt(123))),
      OUTGOING
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("<-[r:Foo|Bar|Baz*]-") {
    val pattern = RelationshipPattern(
      Some(varFor("r")),
      Seq(relTypeName("Foo"), relTypeName("Bar"), relTypeName("Baz")),
      Some(None),
      None,
      None,
      INCOMING
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  test("-[{prop: 'test'}]-") {
    val pattern = RelationshipPattern(
      None,
      Seq.empty,
      None,
      Some(mapOf("prop" -> literalString("test"))),
      None,
      BOTH
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

  // A bit extreme but ensures that the space before WHERE is only added when necessary
  test("-[WHERE false]-") {
    val pattern = RelationshipPattern(
      None,
      Seq.empty,
      None,
      None,
      Some(falseLiteral),
      BOTH
    )(pos)

    patternStringifier(pattern) shouldEqual testName
  }

}
