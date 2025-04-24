// syntax/lexer/tokenizer.gleam

// Tokenizer: Convert raw DSL text into a List(Token),
// handling indentation, comments, strings, and punctuation (including arithmetic ops).
pub module lexer.tokenizer

import gleam/list.{reverse}
import gleam/string
import gleam/int

import lexer.token.{Token, KeywordKind(..), PunctKind(..)}

// Entry: text_lex reads entire source into token_list.
pub fn text_lex(source: String) -> List(Token) {
  let line_list = string.split(source, "\n")
  let {token_list, indent_stack} = line_list_lex(line_list, [], [0])
  let token_list = dedent_emit_remaining(indent_stack, token_list)
  reverse(token_list) ++ [Token.Eof]
}

// Process each line: indent handling, skip comments, lex content, append Newline.
fn line_list_lex(
  line_list: List(String),
  token_list: List(Token),
  indent_stack: List(Int)
) -> {List(Token), List(Int)} {
  case line_list {
    [] -> {token_list, indent_stack}
    [line | rest] ->
      let level = indent_count(line)
      let {token_list, indent_stack} = indent_handle(level, indent_stack, token_list)
      let content = leading_trim(line)
      let token_list =
        if string.starts_with(content, "//") {
          token_list
        } else {
          char_list_lex(string.to_list(content), token_list)
        }
      line_list_lex(rest, [Token.Newline | token_list], indent_stack)
  }
}

// Emit Indent/Dedent tokens based on indent_stack.
fn indent_handle(
  level: Int,
  indent_stack: List(Int),
  token_list: List(Token)
) -> {List(Token), List(Int)} {
  case indent_stack {
    [top | rest] ->
      if level > top {
        let diff = level - top
        {[Token.Indent(diff) | token_list], [level, top | rest]}
      } else if level < top {
        indent_stack_unwind(level, indent_stack, token_list)
      } else {
        {token_list, indent_stack}
      }
    [] -> {token_list, [level]}
  }
}

// Recursively unwind indent_stack to target, emitting Dedent tokens.
fn indent_stack_unwind(
  target: Int,
  indent_stack: List(Int),
  token_list: List(Token)
) -> {List(Token), List(Int)} {
  case indent_stack {
    [top, next | rest] if top > target ->
      let diff = top - next
      indent_stack_unwind(target, [next | rest], [Token.Dedent(diff) | token_list])
    _ -> {token_list, indent_stack}
  }
}

// Emit remaining Dedent tokens at EOF.
fn dedent_emit_remaining(
  indent_stack: List(Int),
  token_list: List(Token)
) -> List(Token) {
  case indent_stack {
    [top, next | rest] ->
      let diff = top - next
      dedent_emit_remaining([next | rest], [Token.Dedent(diff) | token_list])
    _ -> token_list
  }
}

// Lex characters: whitespace, arrow, literals, identifiers, punctuation.
fn char_list_lex(char_list: List(Char), token_list: List(Token)) -> List(Token) {
  case char_list {
    [] -> token_list
    [' ' | rest] -> char_list_lex(rest, token_list)
    ['=', '>' | rest] ->
      // Arrow '->'
      char_list_lex(rest, [Token.Punctuation(Arrow) | token_list])
    [head | rest] ->
      case head {
        '"' ->
          // String literal
          let {chars, after} = string_read(rest, [])
          char_list_lex(after, [Token.StringLiteral(string.from_list(chars)) | token_list])
        ',' -> char_list_lex(rest, [Token.Punctuation(Comma) | token_list])
        ':' -> char_list_lex(rest, [Token.Punctuation(Colon) | token_list])
        '.' -> char_list_lex(rest, [Token.Punctuation(Dot) | token_list])
        '(' -> char_list_lex(rest, [Token.Punctuation(LParen) | token_list])
        ')' -> char_list_lex(rest, [Token.Punctuation(RParen) | token_list])
        '+' -> char_list_lex(rest, [Token.Punctuation(Plus) | token_list])
        '-' -> char_list_lex(rest, [Token.Punctuation(Minus) | token_list])
        '*' -> char_list_lex(rest, [Token.Punctuation(Star) | token_list])
        '/' -> char_list_lex(rest, [Token.Punctuation(Slash) | token_list])
        '=' -> char_list_lex(rest, [Token.Punctuation(Equals) | token_list])
        _ ->
          if char_is_digit(head) {
            let {digits, after} = char_list_read_while(rest, char_is_digit, [head])
            let value = string.from_list(digits) |> Int.parse
            char_list_lex(after, [Token.IntLiteral(value) | token_list])
          } else if char_is_ident_start(head) {
            let {idents, after} = char_list_read_while(rest, char_is_ident, [head])
            let name = string.from_list(idents)
            let tok =
              case keyword_lookup(name) {
                Some(k) -> Token.Keyword(k)
                None -> Token.Identifier(name)
              }
            char_list_lex(after, [tok | token_list])
          } else {
            char_list_lex(rest, token_list)
          }
      }
  }
}

// Predicates for digit and identifier chars
fn char_is_digit(c: Char) -> Bool { '0' <= c && c <= '9' }
fn char_is_ident_start(c: Char) -> Bool {
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
}
fn char_is_ident(c: Char) -> Bool { char_is_ident_start(c) || char_is_digit(c) }

// Read a string literal, handling escape of '"' and '\'.
fn string_read(cs: List(Char), acc: List(Char)) -> {List(Char), List(Char)} {
  case cs {
    [] -> {reverse(acc), []}
    ['\\', ch | rest] -> string_read(rest, [ch | acc])
    ['"' | rest] -> {reverse(acc), rest}
    [ch | rest] -> string_read(rest, [ch | acc])
  }
}

// Collect chars while predicate holds
fn char_list_read_while(
  cs: List(Char),
  pred: fn(Char) -> Bool,
  acc: List(Char>
) -> {List(Char), List(Char)} {
  case cs {
    [ch | rest] if pred(ch) -> char_list_read_while(rest, pred, [ch | acc])
    _ -> {reverse(acc), cs}
  }
}

// Lookup core keywords
fn keyword_lookup(name: String) -> Option(KeywordKind) {
  case name {
    "scope" -> Some(Scope)
    "state" -> Some(State)
    "react" -> Some(React)
    "do"    -> Some(Do)
    "emit"  -> Some(Emit)
    "loop"  -> Some(Loop)
    _ -> None
  }
}

// Count leading spaces
fn indent_count(line: String) -> Int {
  string.to_list(line) |> fn ls -> indent_char_count(ls, 0)
}
fn indent_char_count(cs: List(Char), count: Int) -> Int {
  case cs {
    [' ' | rest] -> indent_char_count(rest, count + 1)
    _ -> count
  }
}

// Trim leading spaces
fn leading_trim(line: String) -> String {
  string.trim_leading(line, " ")
}