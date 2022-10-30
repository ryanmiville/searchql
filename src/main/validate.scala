package searchql

import cats.data.Validated
import cats.data.Validated.*
import cats.syntax.all.*
import Search.*

enum FieldType:
  case StringField, IntField, DoubleField

  def validate[A](clause: Clause[A]): Validated[String, Clause[A]] =
    (this, clause) match
      case (StringField, Clause(_, _, v: String)) =>
        Valid(clause)
      case (IntField, Clause(_, _, v: Int)) =>
        Valid(clause)
      case (DoubleField, Clause(_, _, v: Double)) =>
        Valid(clause)
      case (_, _) =>
        Invalid("wrong type")

type Schema = Map[FieldName, FieldType]

val invalidName = Invalid("invalid field name")

extension (schema: Schema)
  def validate(search: Search): Validated[String, Search] =
    def loop(search: Search): Validated[String, Search] =
      search match
        case c @ Clause(field, _, _) =>
          schema.get(field).fold(invalidName)(_.validate(c))
        case a @ And(left, right) =>
          (loop(left), loop(right)).mapN((_, _) => a)
        case o @ Or(left, right) =>
          (loop(left), loop(right)).mapN((_, _) => o)
    loop(search)
