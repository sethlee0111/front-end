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

class CreateDeletePrivilegeAdministrationCommandParserTest extends AdministrationAndSchemaCommandParserTestBase {

  Seq(
    ("GRANT", "TO", grantGraphPrivilege: noResourcePrivilegeFunc),
    ("DENY", "TO", denyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE GRANT", "FROM", revokeGrantGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE DENY", "FROM", revokeDenyGraphPrivilege: noResourcePrivilegeFunc),
    ("REVOKE", "FROM", revokeGraphPrivilege: noResourcePrivilegeFunc)
  ).foreach {
    case (verb: String, preposition: String, func: noResourcePrivilegeFunc) =>
      Seq(
        ("CREATE", ast.CreateElementAction),
        ("DELETE", ast.DeleteElementAction)
      ).foreach {
        case (createOrDelete, action) =>
          test(s"$verb $createOrDelete ON GRAPH foo $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(graphScopeFoo))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          test(s"$verb $createOrDelete ON GRAPH foo ELEMENTS A $preposition role") {
            yields(func(ast.GraphPrivilege(action, List(graphScopeFoo))(_), List(elemQualifierA), Seq(literalRole)))
          }

          test(s"$verb $createOrDelete ON GRAPH foo NODE A $preposition role") {
            yields(func(ast.GraphPrivilege(action, List(graphScopeFoo))(_), List(labelQualifierA), Seq(literalRole)))
          }

          test(s"$verb $createOrDelete ON GRAPH foo RELATIONSHIPS * $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(graphScopeFoo))(_),
              List(ast.RelationshipAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          // Home graph

          test(s"$verb $createOrDelete ON HOME GRAPH $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          test(s"$verb $createOrDelete ON HOME GRAPH $preposition role1, role2") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(literalRole1, literalRole2)
            ))
          }

          test(s"$verb $createOrDelete ON HOME GRAPH $preposition $$role1, role2") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(paramRole1, literalRole2)
            ))
          }

          test(s"$verb $createOrDelete ON HOME GRAPH RELATIONSHIPS * $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.HomeGraphScope()(_)))(_),
              List(ast.RelationshipAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          // Both Home and * should not parse
          test(s"$verb $createOrDelete ON HOME GRAPH * $preposition role") {
            failsToParse
          }

          // Default graph

          test(s"$verb $createOrDelete ON DEFAULT GRAPH $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          test(s"$verb $createOrDelete ON DEFAULT GRAPH $preposition role1, role2") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(literalRole1, literalRole2)
            ))
          }

          test(s"$verb $createOrDelete ON DEFAULT GRAPH $preposition $$role1, role2") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
              List(ast.ElementsAllQualifier()(_)),
              Seq(paramRole1, literalRole2)
            ))
          }

          test(s"$verb $createOrDelete ON DEFAULT GRAPH RELATIONSHIPS * $preposition role") {
            yields(func(
              ast.GraphPrivilege(action, List(ast.DefaultGraphScope()(_)))(_),
              List(ast.RelationshipAllQualifier()(_)),
              Seq(literalRole)
            ))
          }

          // Both Default and * should not parse
          test(s"$verb $createOrDelete ON DEFAULT GRAPH * $preposition role") {
            failsToParse
          }

          test(s"$verb $createOrDelete ON DATABASE blah $preposition role") {
            val offset = verb.length + createOrDelete.length + 5
            assertFailsWithMessage(
              testName,
              s"""Invalid input 'DATABASE': expected "DEFAULT", "GRAPH", "GRAPHS" or "HOME" (line 1, column ${offset + 1} (offset: $offset))"""
            )
          }
      }
  }
}
