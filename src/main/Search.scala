package searchql

import cats.parse.Parser
import cats.parse.Parser.{string, char as pchar}
import cats.parse.Rfc5234.*

enum Search:
  case Clause(field: String, op: String, value: String)
  case And(left: Search, right: Search)
  case Or(left: Search, right: Search)

object Search:
  def parse(query: String): Either[Parser.Error, Search] =
    parser.parseAll(query)

  private lazy val parser: Parser[Search] =
    val fieldName = (alpha | pchar('_')).rep.string

    val operator = Parser.stringIn(":" :: "~" :: ">" :: "<" :: ">=" :: "<=" :: Nil)

    val str = vchar.repUntil(dquote).string.surroundedBy(dquote)

    val num = string(digit.rep ~ (pchar('.') *> digit.rep).?)
    val fieldValue = str | num

    val notField = pchar('-') *> fieldName ~ pchar(':').as("!=")

    val fieldOp = fieldName ~ operator

    val clause = ((notField | fieldOp) ~ fieldValue).map { case ((field, op), value) => Clause(field, op, value) }

    val and = (sp *> string("and") *> sp *> clause).map(r => And(_, r))
    val or = (sp *> string("or") *> sp *> clause).map(r => Or(_, r))

    val andOr: Parser[Search => Search] = and.backtrack | or

    (clause ~ andOr.rep0).surroundedBy(dquote).map {
      case (c, andOrs) =>
        andOrs.foldLeft[Search](c) { case (acc, fn) => fn(acc) }
    }
  end parser

end Search



