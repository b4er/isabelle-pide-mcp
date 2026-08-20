/*  Title:      PIDE_MCP/pide_mcp_tool.scala
    Author:     Maximilian Schäffeler, Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

abstract class PIDE_MCP_Tool(val name: String) {
  protected var session: PIDE_MCP_Session = _

  def description: String
  def input_schema: JSON.Object.T
  def annotations: Option[JSON.Object.T] = None

  def init(session: PIDE_MCP_Session): Unit = { this.session = session }
  def handle(params: JSON.Object.T): Exn.Result[JSON.T]
  def stop(): Unit = ()
}

abstract class PIDE_MCP_Typed_Tool[A](name: String) extends PIDE_MCP_Tool(name) {
  def input: JSON_Schema.Input.T[A]
  def run(args: A): JSON.T

  final def input_schema: JSON.Object.T = input.schema
  final def handle(params: JSON.Object.T): Exn.Result[JSON.T] =
    Exn.capture {
      val args = input.decode(params).getOrElse(
        throw new Invalid_Params("Arguments do not match the tool's input schema"))
      run(args)
    }
}

class PIDE_MCP_Tools(tools: PIDE_MCP_Tool*) extends Isabelle_System.Service {
  def entries: List[PIDE_MCP_Tool] = tools.toList
}
