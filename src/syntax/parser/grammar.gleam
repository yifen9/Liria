// syntax/parser/grammar.gleam

// Minimal core grammar for Liria DSL with arithmetic ops, attribute access, and integrated loop
pub module parser.grammar

import parser.combinator.{parser_and, parser_bind, parser_map, parser_or, parser_many1, parser_satisfy, parser_token, succeed}
import lexer.token.{Token, KeywordKind(..), PunctKind(..)}
import syntax.ast.{Program, Statement(..), Expr(..), InnerStmt(..)}

// Predicates for indent/dedent
pub fn indent_is(token_item: Token) -> Bool {
  case token_item { Token.Indent(_) -> true; _ -> false }
}

pub fn dedent_is(token_item: Token) -> Bool {
  case token_item { Token.Dedent(_) -> true; _ -> false }
}

// -- Primary expressions: '(', number, or identifier (with dot access)
pub fn parser_expr_primary(token_list: List(Token)) -> Option({Expr, List(Token)}) {
  parser_or(
    // parenthesized: ( expr )
    parser_bind(parser_token(Token.Punctuation(PunctKind.LParen)), fn(_) ->
      parser_bind(parser_expr, fn(inner) ->
        parser_map(
          parser_token(Token.Punctuation(PunctKind.RParen)),
          fn(_) -> inner
        )
      )
    ),
    // integer or identifier
    parser_or(
      parser_map(
        parser_satisfy(fn(t) -> case t { IntLiteral(_) -> true; _ -> false }),
        fn(IntLiteral(v)) -> IntExpr(v)
      ),
      parser_map(
        parser_satisfy(fn(t) -> case t { Identifier(_) -> true; _ -> false }),
        fn(Identifier(n)) -> IdentExpr(n)
      )
    )
  )(token_list)
}

// -- Multiplication / Division
pub fn parser_expr_mul_div(token_list: List(Token)) -> Option({Expr, List(Token)}) {
  parser_bind(parser_expr_primary, fn(left) ->
    fn rec(left_expr: Expr, rest: List(Token)) -> Option({Expr, List(Token)}) {
      parser_or(
        parser_bind(
          parser_satisfy(fn(t) -> case t {
            Token.Punctuation(Star) -> true
            Token.Punctuation(Slash) -> true
            _ -> false
          }), fn(op) ->
            parser_bind(parser_expr_primary, fn(right) ->
              let op_str = case op {
                Token.Punctuation(Star) -> "*"
                Token.Punctuation(Slash) -> "/"
                _ -> ""
              }
              rec(BinaryExpr(left_expr, op_str, right), _)
            )
        ),
        succeed({left_expr, rest})
      )(rest)
    }(left, token_list)
  )(token_list)
}

// -- Addition / Subtraction
pub fn parser_expr_add_sub(token_list: List(Token)) -> Option({Expr, List(Token)}) {
  parser_bind(parser_expr_mul_div, fn(left) ->
    fn rec(left_expr: Expr, rest: List(Token)) -> Option({Expr, List(Token)}) {
      parser_or(
        parser_bind(
          parser_satisfy(fn(t) -> case t {
            Token.Punctuation(Plus) -> true
            Token.Punctuation(Minus) -> true
            _ -> false
          }), fn(op) ->
            parser_bind(parser_expr_mul_div, fn(right) ->
              let op_str = case op {
                Token.Punctuation(Plus) -> "+"
                Token.Punctuation(Minus) -> "-"
                _ -> ""
              }
              rec(BinaryExpr(left_expr, op_str, right), _)
            )
        ),
        succeed({left_expr, rest})
      )(rest)
    }(left, token_list)
  )(token_list)
}

// full expression entry: only arithmetic and attribute access
pub fn parser_expr(token_list: List(Token)) -> Option({Expr, List(Token)}) {
  parser_expr_add_sub(token_list)
}

// Parser for identifier names
pub fn parser_name_identifier(token_list: List(Token)) -> Option({String, List(Token)}) {
  parser_map(
    parser_satisfy(fn(t) -> case t { Identifier(_) -> true; _ -> false }),
    fn(Identifier(name)) -> name
  )(token_list)
}

// -- Inner statements --
// 'do' statement
pub fn parser_stmt_do(token_list: List(Token)) -> Option({InnerStmt, List(Token)}) {
  parser_map(
    parser_and(parser_token(Keyword(Do)), parser_token(Token.Newline)),
    fn({_, _}) -> DoStmt
  )(token_list)
}

// 'emit' statement with optional count
pub fn parser_stmt_emit(token_list: List(Token)) -> Option({InnerStmt, List(Token)}) {
  parser_bind(parser_token(Keyword(Emit)), fn(_) ->
    parser_bind(parser_name_identifier, fn(target) ->
      parser_or(
        parser_map(
          parser_and(
            parser_and(parser_token(Token.Punctuation(Arrow)), parser_expr),
            parser_token(Token.Newline)
          ),
          fn({{_, expr}, _}) -> EmitStmt(target, Some(expr))
        ),
        parser_map(parser_token(Token.Newline), fn(_) -> EmitStmt(target, None))
      )
    )
  )(token_list)
}

// 'loop' statement: loop <expr> do/newline
pub fn parser_stmt_loop(token_list: List(Token)) -> Option({InnerStmt, List(Token)}) {
  parser_map(
    parser_and(
      parser_and(parser_token(Keyword(Loop)), parser_expr),
      parser_token(Token.Newline)
    ),
    fn({{_, expr}, _}) -> LoopStmt(expr)
  )(token_list)
}

// choice of inner stmt
pub fn parser_stmt_inner(token_list: List(Token)) -> Option({InnerStmt, List(Token)}) {
  parser_or(parser_stmt_do,
    parser_or(parser_stmt_emit, parser_stmt_loop)
  )(token_list)
}

// -- Top-level statements --
// 'react' block
pub fn parser_stmt_react(token_list: List(Token)) -> Option({Statement, List(Token)}) {
  parser_bind(parser_token(Keyword(React)), fn(_) ->
    parser_bind(parser_name_identifier, fn(event) ->
      parser_bind(parser_token(Token.Punctuation(Colon)), fn(_) ->
        parser_bind(parser_token(Token.Newline), fn(_) ->
          parser_bind(parser_satisfy(indent_is), fn(_) ->
            parser_bind(parser_many1(parser_stmt_inner), fn(body_list) ->
              parser_map(
                parser_satisfy(dedent_is),
                fn(_) -> ReactStmt(event, List.map(fn({st, _}) -> st, body_list))
              )
            )
          )
        )
      )
    )
  )(token_list)
}

// 'state' assignment
pub fn parser_stmt_state(token_list: List(Token)) -> Option({Statement, List(Token)}) {
  parser_map(
    parser_and(
      parser_and(
        parser_and(parser_token(Keyword(State)), parser_satisfy(fn(t) -> case t { Identifier(_) -> true; _ -> false })),
        parser_token(Token.Punctuation(Equals))
      ),
      parser_and(parser_expr, parser_token(Token.Newline))
    ),
    fn({{{_, Identifier(name)}, _}, {expr, _}}) -> StateStmt(name, expr)
  )(token_list)
}

// any top-level stmt
pub fn parser_stmt(token_list: List(Token)) -> Option({Statement, List(Token)}) {
  parser_or(parser_stmt_state, parser_stmt_react)(token_list)
}

// program: one or more statements
pub let parser_program: Parser(Program) = parser_map(
  parser_many1(parser_stmt),
  fn(stmt_list) -> List.map(fn({st, _}) -> st, stmt_list)
)