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
package org.opencypher.v9_0.ast.factory.neo4j.privilege

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.ast.factory.neo4j.AdministrationAndSchemaCommandParserTestBase

class WritePrivilegeAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  Seq(
    ("GRANT", "TO", grantGraphPrivilege: noResourcePrivilegeFunc),
    ("DENY", "TO", denyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE", "FROM", revokeGraphPrivilege: noResourcePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: noResourcePrivilegeFunc) =>
      test(s"$verb WRITE ON GRAPH foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPHS foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      // Multiple graphs should be allowed (with and without plural GRAPHS)

      test(s"$verb WRITE ON GRAPH * $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(ast.AllGraphsScope()(_)))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPHS * $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(ast.AllGraphsScope()(_)))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPH foo, baz $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo, graphScopeBaz))(pos),
          List(ast.ElementsAllQualifier() _),
          List(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPHS foo, baz $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo, graphScopeBaz))(pos),
          List(ast.ElementsAllQualifier() _),
          List(literalRole)
        ))
      }

      // Default and home graph should parse

      test(s"$verb WRITE ON HOME GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(ast.HomeGraphScope()(_)))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON DEFAULT GRAPH $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(ast.DefaultGraphScope()(_)))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      // Multiple roles should be allowed

      test(s"$verb WRITE ON GRAPH foo $preposition role1, role2") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo))(_),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole1, literalRole2)
        ))
      }

      // Parameters and escaped strings should be allowed

      test(s"$verb WRITE ON GRAPH $$foo $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeParamFoo))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPH `f:oo` $preposition role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(ast.NamedGraphScope(literalFColonOo)(_)))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRole)
        ))
      }

      test(s"$verb WRITE ON GRAPH foo $preposition $$role") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(paramRole)
        ))
      }

      test(s"$verb WRITE ON GRAPH foo $preposition `r:ole`") {
        yields(func(
          ast.GraphPrivilege(ast.WriteAction, List(graphScopeFoo))(pos),
          List(ast.ElementsAllQualifier() _),
          Seq(literalRColonOle)
        ))
      }

      // Resource or qualifier should not be supported

      test(s"$verb WRITE {*} ON GRAPH foo $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE {prop} ON GRAPH foo $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo NODE A $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo NODES * $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo RELATIONSHIP R $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo RELATIONSHIPS * $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo ELEMENT A $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo ELEMENTS * $preposition role") {
        failsToParse
      }

      // Invalid/missing part of the command

      test(s"$verb WRITE ON GRAPH f:oo $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo $preposition ro:le") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH foo $preposition") {
        failsToParse
      }

      test(s"$verb WRITE GRAPH foo $preposition role") {
        failsToParse
      }

      // DEFAULT and HOME together with plural GRAPHS

      test(s"$verb WRITE ON HOME GRAPHS $preposition role") {
        val offset = verb.length + 15
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'GRAPHS': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb WRITE ON DEFAULT GRAPHS $preposition role") {
        val offset = verb.length + 18
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'GRAPHS': expected "GRAPH" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      // Default and home graph with named graph

      test(s"$verb WRITE ON HOME GRAPH baz $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON DEFAULT GRAPH baz $preposition role") {
        failsToParse
      }

      // Mix of specific graph and *

      test(s"$verb WRITE ON GRAPH foo, * $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON GRAPH *, foo $preposition role") {
        failsToParse
      }

      // Database instead of graph keyword

      test(s"$verb WRITE ON DATABASES * $preposition role") {
        val offset = verb.length + 10
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DATABASES': expected "DEFAULT", "GRAPH", "GRAPHS" or "HOME" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb WRITE ON DATABASE foo $preposition role") {
        val offset = verb.length + 10
        assertFailsWithMessage(
          testName,
          s"""Invalid input 'DATABASE': expected "DEFAULT", "GRAPH", "GRAPHS" or "HOME" (line 1, column ${offset + 1} (offset: $offset))"""
        )
      }

      test(s"$verb WRITE ON HOME DATABASE $preposition role") {
        failsToParse
      }

      test(s"$verb WRITE ON DEFAULT DATABASE $preposition role") {
        failsToParse
      }
  }
}
