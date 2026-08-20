/*  Title:      PIDE_MCP/tool_find_entities.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Find_Entities {
  case class Args(
    origin: String,
    start_line: Int,
    end_line: Option[Int],
    snippet_lines: Int,
    filter_origins: List[String]
  )
}

class Tool_Find_Entities extends PIDE_MCP_Typed_Tool[Tool_Find_Entities.Args]("find_entities") {
  import JSON_Schema.Input
  import PIDE_MCP_Tool_Schema.{origin, start_line, end_line_opt}

  private val snippet_preview_lines: Int = 3

  private val definition_kinds =
    List(Markup.AXIOM, Markup.FACT, Markup.DYNAMIC_FACT, Markup.LITERAL_FACT,
      Markup.CONSTANT, Markup.TYPE_NAME,
      Markup.THEORY, Markup.SESSION, Markup.CLASS, Markup.LOCALE,
      Markup.COMMAND, Markup.CASE, Markup.BUNDLE,
      Markup.METHOD, Markup.ATTRIBUTE,
      Markup.ML_ANTIQUOTATION, Markup.ML_DEF,
      Markup.DOCUMENT_ANTIQUOTATION, Markup.DOCUMENT_ANTIQUOTATION_OPTION)

  def description: String =
    "Look up what entities (constants, theorems, commands, methods, ML terms,...) in a file are defined where and how, i.e. find their origin with preview snippets. "
      + "Useful to learn more about concepts that you are uncertain about or for which you need more information (e.g. the actual theorem statement) and to study the content of a file. To get all entities defined in a file, select the file with full range and also pass it in filter_origins. "
      + PIDE_MCP_Tool_Schema.implicit_load_file
      + " Implicitly (re)loads theories containing source snippets if required."

  private val snippet_lines_f = Input.default("snippet_lines",
    "Number of context lines per definition source snippet (use 0 to omit).",
    Input.at_least(0), snippet_preview_lines)
  private val filter_origins_f = Input.optional("filter_origins",
    "List of origins (session-qualified theory names or file paths). If provided, only returns entities whose definition originates from one of these. Use this if you want to explore the entities defined by a given origin.",
    Input.array(Input.string))

  val input: Input.T[Tool_Find_Entities.Args] =
    Input.record(List(origin, start_line, end_line_opt, snippet_lines_f, filter_origins_f)) { obj =>
      Tool_Find_Entities.Args(origin.get(obj), start_line.get(obj), end_line_opt.get(obj),
        snippet_lines_f.get(obj), filter_origins_f.get(obj).getOrElse(Nil))
    }

  override def annotations: Option[JSON.Object.T] = Some(JSON.Object("readOnlyHint" -> true))

  def run(args: Tool_Find_Entities.Args): JSON.T = {
    val node_name = Exn.release(session.node_name(args.origin))
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name)
    val doc = Line.Document(snapshot.node.source)
    val (s, e) =
      Exn.release(PIDE_MCP_Tool_Util.resolve_lines(Some(args.start_line), args.end_line, doc.lines.length))
    val filter_origins =
      args.filter_origins.map(s => session.origin(Exn.release(session.node_name(s)))).toSet
    val range = PIDE_MCP_Util.range(doc, s, e)
    Exn.release(PIDE_MCP_Commands.definitions_json(session, snapshot, Some(range), args.snippet_lines,
      filter_origins, definition_kinds,
      "The definition entry has not been loaded yet. " + PIDE_MCP_Tool_Util.retry_soon_message))
  }
}

class Tools_Find_Entities extends PIDE_MCP_Tools(new Tool_Find_Entities)
