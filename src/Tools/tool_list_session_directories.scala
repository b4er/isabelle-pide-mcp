/*  Title:      PIDE_MCP/tool_list_session_directories.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_List_Session_Directories {
  def session_directories(session: PIDE_MCP_Session): List[Path] =
    Sessions.directories(session.dirs, Nil).map(p => PIDE_MCP_Util.canonical_path(p._2))
}

class Tool_List_Session_Directories extends PIDE_MCP_Typed_Tool[Unit]("list_session_directories") {
  def description: String =
    "List all directories from which the Isabelle PIDE session attempts to load files. "
      + "Use this when you want to learn what libraries you have access to and where they are located. "
      + "Note that session names, which you have to use for session-qualified loading of (library) theories, are stored in the ROOT files of these directories."

  val input: JSON_Schema.Input.T[Unit] = JSON_Schema.Input.record(Nil)(_ => ())

  override def annotations: Option[JSON.Object.T] = Some(JSON.Object("readOnlyHint" -> true))

  def run(args: Unit): JSON.T =
    Tool_List_Session_Directories.session_directories(session).map(_.implode)
}

class Tools_List_Session_Directories extends PIDE_MCP_Tools(new Tool_List_Session_Directories)
