/*  Title:      PIDE_MCP/pide_mcp_json_schema.scala
    Author:     Basil Fürer

Typed combinators for JSON-Schema generation and decoding of tool inputs.
*/

package isabelle.pide.mcp

import isabelle._

final class Invalid_Params(val message: String) extends RuntimeException(message)

object JSON_Schema {
  private def typed(typ: String, extra: (String, JSON.T)*): JSON.Object.T =
    JSON.Object((("type" -> typ) +: extra): _*)

  private def object_schema(
    props: List[(String, JSON.T)],
    required: List[String],
    description: String
  ): JSON.Object.T =
    JSON.Object((List[(String, JSON.T)](
      "type" -> "object",
      "properties" -> JSON.Object(props: _*)) ++
      (if (description.nonEmpty) List("description" -> description) else Nil) ++
      (if (required.nonEmpty) List("required" -> required) else Nil)): _*)

  object Input {
    final case class T[A](schema: JSON.Object.T, decode: JSON.T => Option[A]) {
      def describe(desc: String): T[A] = keyword("description", desc)
      def keyword(k: String, v: JSON.T): T[A] = copy(schema = schema + (k -> v))
      def map[B](f: A => B): T[B] = copy(decode = decode(_).map(f))
      def emap[B](f: A => Option[B]): T[B] = copy(decode = decode(_).flatMap(f))
    }

    private def prim[A](typ: String, unapply: JSON.T => Option[A]): T[A] =
      T(typed(typ), unapply)

    val string: T[String] = prim("string", JSON.Value.String.unapply)
    val integer: T[Int] = prim("integer", JSON.Value.Int.unapply)
    val boolean: T[Boolean] = prim("boolean", JSON.Value.Boolean.unapply)

    private def sequence[A](xs: List[Option[A]]): Option[List[A]] =
      xs.foldRight(Option(List.empty[A])) { case (x, acc) =>
        for {a <- x; as <- acc} yield a :: as
      }

    def array[A](elem: T[A]): T[List[A]] =
      T(typed("array", "items" -> elem.schema), {
        case xs: List[JSON.T] @unchecked => sequence(xs.map(elem.decode))
        case _ => None
      })

    def enumerate(values: List[String]): T[String] =
      string.keyword("enum", values).emap(v => Option.when(values.contains(v))(v))

    def at_least(n: Int): T[Int] =
      integer.keyword("minimum", n).emap(i => Option.when(i >= n)(i))

    private def invalid_value_msg(name: String, schema: JSON.Object.T, got: JSON.T): String = {
      val typ = JSON.string(schema, "type").getOrElse("value")
      val constraint =
        schema.get("enum").map(e => ", expected one of " + JSON.Format(e))
          .orElse(schema.get("minimum").map(m => ", expected >= " + JSON.Format(m)))
          .getOrElse("")
      s"invalid value for parameter '$name': expected type '$typ'$constraint, got " + JSON.Format(got)
    }

    sealed abstract class Field[A] {
      def name: String
      def required: Boolean
      def schema: JSON.T
      def decode(p: JSON.Object.T): Option[A]
      def get(p: JSON.Object.T): A

      protected def present[B](p: JSON.Object.T, t: T[B]): Option[B] =
        JSON.value(p, name).map(j =>
          t.decode(j).getOrElse(throw new Invalid_Params(invalid_value_msg(name, t.schema, j))))
    }

    final case class Required[A](name: String, t: T[A]) extends Field[A] {
      def required = true
      def schema: JSON.T = t.schema
      def decode(p: JSON.Object.T): Option[A] = present(p, t)
      def get(p: JSON.Object.T): A =
        present(p, t).getOrElse(throw new Invalid_Params("Missing required parameter: " + name))
    }

    final case class Optional[A](name: String, t: T[A]) extends Field[Option[A]] {
      def required = false
      def schema: JSON.T = t.schema
      def decode(p: JSON.Object.T): Option[Option[A]] = Some(present(p, t))
      def get(p: JSON.Object.T): Option[A] = present(p, t)
    }

    final case class With_Default[A](name: String, t: T[A], default: A) extends Field[A] {
      def required = false
      def schema: JSON.T = t.schema + ("default" -> default)
      def decode(p: JSON.Object.T): Option[A] = Some(present(p, t).getOrElse(default))
      def get(p: JSON.Object.T): A = present(p, t).getOrElse(default)
    }

    def required[A](name: String, desc: String, t: T[A]): Field[A] =
      Required(name, t.describe(desc))

    def optional[A](name: String, desc: String, t: T[A]): Field[Option[A]] =
      Optional(name, t.describe(desc))

    def default[A](name: String, desc: String, t: T[A], v: A): Field[A] =
      With_Default(name, t.describe(desc), v)

    def obj(fields: List[Field[_]], description: String = ""): JSON.Object.T =
      object_schema(fields.map(f => f.name -> f.schema), fields.filter(_.required).map(_.name), description)

    def record[A](fields: List[Field[_]], desc: String = "")(build: JSON.Object.T => A): T[A] =
      T(obj(fields, desc), j => JSON.Object.unapply(j).map(build))

    def record_opt[A](fields: List[Field[_]], desc: String = "")(build: JSON.Object.T => Option[A]): T[A] =
      T(obj(fields, desc), j => JSON.Object.unapply(j).flatMap(build))
  }
}
