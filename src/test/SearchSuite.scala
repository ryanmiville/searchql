package searchql

class SearchSuite extends munit.FunSuite {
  test("Parsing valid query") {
    val query = """"domain_name:"adobe" or product_name~"adobe" and score<=80""""
    assert(Search.parse(query).isRight)
  }

  test("Parsing invalid query") {
    val query = """"domain_name:"adobe" or product_name~adobe" and score<=80""""
    assert(Search.parse(query).isLeft)
  }

}
