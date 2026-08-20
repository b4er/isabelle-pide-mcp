/*  Title:      PIDE_MCP/pide_mcp_tool_schema.scala
    Author:     Kevin Kappelmann

Shared typed schema fields for MCP tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Schema {
  import JSON_Schema.Input

  val implicit_load_file: String = "Implicitly loads the file if required."
  val implicit_reload_file: String = "Implicitly (re)loads the file."

  val origin: Input.Field[String] =
    Input.required("origin",
      "Session-qualified theory name (e.g. \"HOL.Nat\") or file path (e.g. \"foo.ML\")",
      Input.string)
  val start_line: Input.Field[Int] =
    Input.required("start_line", "First line to include.", Input.at_least(1))
  val start_line_opt: Input.Field[Option[Int]] =
    Input.optional("start_line", "First line to include.",
      Input.at_least(1).keyword("default", 1))
  val end_line_opt: Input.Field[Option[Int]] =
    Input.optional("end_line", "Last line to include (default: end of file).",
      Input.at_least(1))
}
