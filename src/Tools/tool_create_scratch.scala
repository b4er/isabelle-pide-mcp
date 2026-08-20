/*  Title:      PIDE_MCP/tool_create_scratch.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._
import java.io.{File => JFile}

object Tool_Create_Scratch {
  case class Args(name_suffix: Option[String], extension: Option[String])
}

class Tool_Create_Scratch extends PIDE_MCP_Typed_Tool[Tool_Create_Scratch.Args]("create_scratch") {
  import JSON_Schema.Input

  def description: String =
    "Create a temporary file for experimentation that does not interfere with user accessible files. "
      + "Use this whenever you think you need to do iterative developments or when you want to find and explore theorems, syntax, concepts, commands, ML code, etc. Write back final results to files accessible to the user. "
      + "Temporary files are cleaned up when the session stops."

  private val name_suffix_f = Input.optional("name_suffix",
    "Label to identify the scratch file (auto-generated if omitted)", Input.string)
  private val extension_f = Input.optional("extension",
    "File extension (typically \".thy\" or \".ML\")", Input.string)

  val input: Input.T[Tool_Create_Scratch.Args] =
    Input.record(List(name_suffix_f, extension_f)) { obj =>
      Tool_Create_Scratch.Args(name_suffix_f.get(obj), extension_f.get(obj))
    }

  private val scratch_prefix: String = "tmp_pide_mcp_scratch_"
  private val scratch_tmpdir_prefix: String = "tmp_pide_mcp_scratch"
  private val scratch_dir = Synchronized[Option[JFile]](None)

  private def get_scratch_dir(): JFile =
    scratch_dir.change_result {
      case some @ Some(dir) if dir.isDirectory => (dir, some)
      case _ =>
        val dir = Isabelle_System.tmp_dir(scratch_tmpdir_prefix)
        (dir, Some(dir))
    }

  override def stop(): Unit =
    scratch_dir.change {
      case Some(dir) => Isabelle_System.rm_tree(dir); None
      case None => None
    }

  def create_scratch(
    name_suffix: Option[String] = None,
    extension: Option[String] = None
  ): Exn.Result[Path] = {
    val suffix = name_suffix.getOrElse(Date.now().format(Date.Format("yyyy_MM_dd_HH_mm_ss_SSS")))
    val base_name = scratch_prefix + suffix
    val file_name = extension match { case Some(ext) => base_name + ext case None => base_name }
    Exn.capture {
      val file_path = PIDE_MCP_Util.canonical_path(File.path(get_scratch_dir()) + Path.basic(file_name))
      File.write(file_path, "")
      file_path
    }
  }

  def run(args: Tool_Create_Scratch.Args): JSON.T = {
    val path = Exn.release(create_scratch(args.name_suffix, args.extension))
    JSON.Object("path" -> path.implode)
  }
}

class Tools_Create_Scratch extends PIDE_MCP_Tools(new Tool_Create_Scratch)
