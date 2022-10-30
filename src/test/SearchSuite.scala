package searchql

class SearchSuite extends munit.FunSuite {
  test("Parsing valid query") {
    val query =
      """"domain_name:"adobe" or product_name~"adobe" and score<=80""""
    assert(Search.parse(query).isRight)
  }

  test("Parsing invalid query") {
    val query = """"domain_name:"adobe" or product_name~adobe" and score<=80""""
    assert(Search.parse(query).isLeft)
  }

  test("Validating valid query") {
    val query =
      """"domain_name:"adobe" or product_name~"adobe" and score<=80""""
    val schema: Schema =
      Map(
        "domain_name" -> FieldType.StringField,
        "product_name" -> FieldType.StringField,
        "score" -> FieldType.IntField
      )

    assert(Search.parse(query).flatMap(schema.validate(_).toEither).isRight)
  }

  test("Validating invalid query") {
    val query =
      """"domain_name:"adobe" or product_name~"adobe" and score<=80""""
    val schema: Schema =
      Map(
        "domain_name" -> FieldType.StringField,
        "score" -> FieldType.StringField
      )

    assert(Search.parse(query).flatMap(schema.validate(_).toEither).isLeft)
  }
}
