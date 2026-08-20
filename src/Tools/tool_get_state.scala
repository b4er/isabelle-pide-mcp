/*  Title:      PIDE_MCP/tool_get_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Get_State {
  case class Args(
    origin: String,
    start_line: Option[Int],
    end_line: Option[Int],
    include_types: Boolean,
    include_facts: Boolean,
    include_infos: Boolean,
    include_full_markup: Boolean,
    commands_limit: Option[Int]
  )
}

class Tool_Get_State extends PIDE_MCP_Typed_Tool[Tool_Get_State.Args]("get_state") {
  import JSON_Schema.Input
  import PIDE_MCP_Tool_Schema.{origin, start_line_opt, end_line_opt}

  def description: String =
    "Inspect the state of a (range in a) file: goals, variables, errors, warnings, etc. "
      + "Returns a summary (errors, warnings, commands still running, total timing,...) and details for all commands in range. "
      + "**Use this frequently to check if you are making progress, what is left to be done, and importantly, if certain commands are still processing, potentially even looping.** "
      + "For files, the response has a single command entry derived from the command that loaded it in the respective theory. "
      + PIDE_MCP_Tool_Schema.implicit_load_file

  private val include_types_f = Input.default("include_types",
    "Include types (Isabelle and ML) for variables and constants.", Input.boolean, false)
  private val include_facts_f = Input.default("include_facts",
    "Include facts/theorems used in range.", Input.boolean, false)
  private val include_infos_f = Input.default("include_infos",
    "Include writeln and information output in the state. This can get large, but often contains useful information. Avoid using it for large ranges if possible.",
    Input.boolean, false)
  private val include_full_markup_f = Input.default("include_full_markup",
    "Include full PIDE markup information. This gets very large - **use only sparingly and very targeted to get local details**.",
    Input.boolean, false)
  private val commands_limit_f = Input.optional("commands_limit",
    "Maximum number of commands to return. Omit to return all commands. "
      + "**Set to 0 whenever you only want to get summary information (e.g. to see if there are any errors, warnings, etc.).** "
      + "Note that the returned summary will count all commands, even the truncated ones. "
      + "Warning: for large ranges, there are thousands of commands that may flood your context. Set the commands limit to 0 if you do not need details.",
    Input.integer)

  val input: Input.T[Tool_Get_State.Args] =
    Input.record(List(origin, start_line_opt, end_line_opt, include_types_f, include_facts_f,
      include_infos_f, include_full_markup_f, commands_limit_f)) { obj =>
      Tool_Get_State.Args(origin.get(obj), start_line_opt.get(obj), end_line_opt.get(obj),
        include_types_f.get(obj), include_facts_f.get(obj), include_infos_f.get(obj),
        include_full_markup_f.get(obj), commands_limit_f.get(obj))
    }

  override def annotations: Option[JSON.Object.T] = Some(JSON.Object("readOnlyHint" -> true))

  def run(args: Tool_Get_State.Args): JSON.T = {
    val node_name = Exn.release(session.node_name(args.origin))
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name)
    val doc = Line.Document(snapshot.node.source)
    val (s, e) =
      Exn.release(PIDE_MCP_Tool_Util.resolve_lines(args.start_line, args.end_line, doc.lines.length))

    val range = PIDE_MCP_Util.range(doc, s, e)
    val entries =
      (if (node_name.is_theory) {
        if (session.is_base_session_theory(node_name))
          Exn.release(PIDE_MCP_Commands.state_entries_theory_base_session(snapshot, Some(range)))
        else PIDE_MCP_Commands.state_entries_theory_dynamic(snapshot, Some(range))
      } else PIDE_MCP_Commands.state_entry_file(snapshot, Some(range)).iterator)
      .toList
    val opts = PIDE_MCP_Commands.State_Options(args.include_types, args.include_facts,
      args.include_infos, args.include_full_markup)
    val command_states = PIDE_MCP_Commands.state_entries_json(snapshot, entries, doc, opts)
    val summary = PIDE_MCP_Commands.state_summary_json(command_states)
    val command_states_limited = args.commands_limit.map(command_states.take).getOrElse(command_states)
    PIDE_MCP_Commands.prefix_command_status_keys(summary) +
      ("commands" -> JSON.Object("count" -> command_states.length,
        "count_returned" -> command_states_limited.length, "commands" -> command_states_limited))
  }
}

class Tools_Get_State extends PIDE_MCP_Tools(new Tool_Get_State)
