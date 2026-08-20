/*  Title:      PIDE_MCP/tool_read.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Read {
  case class Args(origin: String, start_line: Option[Int], end_line: Option[Int])
}

class Tool_Read extends PIDE_MCP_Typed_Tool[Tool_Read.Args]("read") {
  import JSON_Schema.Input
  import PIDE_MCP_Tool_Schema.{origin, start_line_opt, end_line_opt}

  def description: String =
    "Read line-numbered content for a given range. Use this to get the file's content. Also use it to re-synchronise the file's disk content with the PIDE session if you experience discrepancies between the PIDE state and the external disk state (e.g. due to external edits). " + PIDE_MCP_Tool_Schema.implicit_reload_file

  val input: Input.T[Tool_Read.Args] =
    Input.record(List(origin, start_line_opt, end_line_opt)) { obj =>
      Tool_Read.Args(origin.get(obj), start_line_opt.get(obj), end_line_opt.get(obj))
    }

  override def annotations: Option[JSON.Object.T] = Some(JSON.Object("readOnlyHint" -> true))

  def run(args: Tool_Read.Args): JSON.T = {
    val node_name = Exn.release(session.node_name(args.origin))
    val text = Exn.release(session.read_update_resolve(node_name))
    val lines_count = Line.Document(text).lines.length
    val (s, e) =
      Exn.release(PIDE_MCP_Tool_Util.resolve_lines(args.start_line, args.end_line, lines_count))
    session.await_stable_snapshot()
    PIDE_MCP_Util.numbered_lines_range(text, s, e)
  }
}

class Tools_Read extends PIDE_MCP_Tools(new Tool_Read)
