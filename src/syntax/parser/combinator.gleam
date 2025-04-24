// syntax/parser/combinator.gleam

// Parser combinators for Liria DSL.
// Each combinator works on a list of Tokens, producing optional parsed value and remaining tokens.
pub module parser.combinator

import lexer.token.{Token}

// Parser type: takes Token list, returns optional {Value, Remaining tokens}.
pub type Parser(a) = fn(tokens: List(Token)) -> Option({a, List(Token)})

// Always succeed with given value, consuming no tokens.
pub fn succeed(value: a) -> Parser(a) {
  fn(tokens) -> Some({value, tokens})
}

// Always fail, consuming no tokens.
pub fn fail() -> Parser(a) {
  fn(_) -> None
}

// Map parsed value.
pub fn parser_map(
  parser: Parser(a),
  f: fn(a) -> b
) -> Parser(b) {
  fn(tokens) {
    case parser(tokens) {
      Some({value, rest}) -> Some({f(value), rest})
      None -> None
    }
  }
}

// Sequence two parsers, returning a tuple of their results.
pub fn parser_and(
  p1: Parser(a),
  p2: Parser(b)
) -> Parser({a, b}) {
  fn(tokens) {
    case p1(tokens) {
      Some({v1, rest1}) ->
        case p2(rest1) {
          Some({v2, rest2}) -> Some({{v1, v2}, rest2})
          None -> None
        }
      None -> None
    }
  }
}

// Try first parser; if it fails, backtrack and try second.
pub fn parser_or(
  p1: Parser(a),
  p2: Parser(a)
) -> Parser(a) {
  fn(tokens) {
    case p1(tokens) {
      Some(res) -> Some(res)
      None -> p2(tokens)
    }
  }
}

// Bind: feed result of first into next parser generator.
pub fn parser_bind(
  p: Parser(a),
  f: fn(a) -> Parser(b)
) -> Parser(b) {
  fn(tokens) {
    case p(tokens) {
      Some({v, rest}) -> f(v)(rest)
      None -> None
    }
  }
}

// Zero or more repetitions.
pub fn parser_many(
  p: Parser(a)
) -> Parser(List(a)) {
  fn(tokens) {
    let rec = parser_many(p)
    case p(tokens) {
      Some({v, rest}) ->
        case rec(rest) {
          Some({vs, final}) -> Some({[v | vs], final})
          None -> Some({[v], rest})
        }
      None -> Some({[], tokens})
    }
  }
}

// One or more repetitions.
pub fn parser_many1(
  p: Parser(a)
) -> Parser(List(a)) {
  parser_bind(p, fn(first) ->
    parser_map(parser_many(p), fn(rest) -> [first | rest])
  )
}

// Zero or more separated by sep parser.
pub fn parser_sep_by(
  p: Parser(a),
  sep: Parser(_)
) -> Parser(List(a)) {
  parser_map(
    parser_bind(p, fn(first) ->
      parser_map(parser_many(parser_bind(sep, fn(_) -> p)), fn(rest) -> [first | rest])
    ),
    fn(list) -> list
  )
}

// Consume a single token if it satisfies predicate.
pub fn parser_satisfy(
  pred: fn(Token) -> Bool
) -> Parser(Token) {
  fn(tokens) {
    case tokens {
      [head | tail] if pred(head) -> Some({head, tail})
      _ -> None
    }
  }
}

// Match a specific token (by equality).
pub fn parser_token(
  expected: Token
) -> Parser(Token) {
  parser_satisfy(fn(t) -> t == expected)
}