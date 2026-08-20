/*  Title:      PIDE_MCP/tool_create_file.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Create_File {
  def create_file(path: Path): Exn.Result[Boolean] = Exn.capture {
    val abs_path = PIDE_MCP_Util.canonical_path(path)
    if (abs_path.file.isDirectory) error("Path " + abs_path.implode + " is an existing directory.")
    Isabelle_System.make_directory(abs_path.dir)
    if (!abs_path.file.exists) { File.write(abs_path, ""); true }
    else false
  }
}

class Tool_Create_File extends PIDE_MCP_Typed_Tool[String]("create_file") {
  import JSON_Schema.Input

  def description: String =
    "Create an empty file at the given path. "
      + "Creates missing parent directories if necessary. "
      + "If the file already exists, it does nothing."

  private val path_f = Input.required("path",
    "File path to create (e.g. \"./Algebra/algebra_simp.ML\" or \"/path/to/My_Theory.thy\")",
    Input.string)

  val input: Input.T[String] = Input.record(List(path_f))(path_f.get)

  def run(path: String): JSON.T = {
    val created = Exn.release(Tool_Create_File.create_file(Path.explode(path)))
    if (created) "File created" else "File already exists"
  }
}

class Tools_Create_File extends PIDE_MCP_Tools(new Tool_Create_File)
