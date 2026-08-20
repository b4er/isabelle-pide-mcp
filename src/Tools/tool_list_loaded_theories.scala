/*  Title:      PIDE_MCP/tool_list_loaded_theories.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_List_Loaded_Theories {
  def loaded_theories(session: PIDE_MCP_Session, nodes: Document.Nodes)
    : (List[Document.Node.Name], List[Document.Node.Name]) = {
    val all_nodes = nodes.topological_order.filter(_.is_theory)
    val (base_session, dynamic) = all_nodes.partition(session.is_base_session_theory)
    (base_session, dynamic.filter(PIDE_MCP_Util.is_loaded_dynamic(nodes, _)))
  }
}

class Tool_List_Loaded_Theories extends PIDE_MCP_Typed_Tool[Unit]("list_loaded_theories") {
  def description: String = "List all loaded theories from the session."

  val input: JSON_Schema.Input.T[Unit] = JSON_Schema.Input.record(Nil)(_ => ())

  override def annotations: Option[JSON.Object.T] = Some(JSON.Object("readOnlyHint" -> true))

  def run(args: Unit): JSON.T = {
    val (base_session, dynamic) =
      Tool_List_Loaded_Theories.loaded_theories(session, Exn.release(session.tip_version()).nodes)
    def to_entry(node_name: Document.Node.Name) = JSON.Object("origin" -> session.origin(node_name))
    JSON.Object(
      "dynamic" -> dynamic.map(to_entry),
      "base_session" -> base_session.map(to_entry)
    )
  }
}

class Tools_List_Loaded_Theories extends PIDE_MCP_Tools(new Tool_List_Loaded_Theories)
