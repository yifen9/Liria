// syntax/lexer/token.gleam

// Defines punctuation kinds (minimal set) and core keywords,
// plus the Token ADT for Liria DSL.
pub type PunctKind
  = Comma
  | Colon
  | Dot
  | LParen
  | RParen
  | Equals
  | Arrow
  | Plus
  | Minus
  | Star
  | Slash

pub type KeywordKind
  = Scope    // 'scope'
  | State    // 'state'
  | React    // 'react'
  | Do       // 'do'
  | Emit     // 'emit'
  | Loop     // 'loop'

pub type Token
  = Identifier(String)      // user-defined names
  | Keyword(KeywordKind)    // core DSL keywords
  | IntLiteral(Int)         // integer literals
  | StringLiteral(String)   // quoted strings
  | Punctuation(PunctKind)  // punctuation tokens
  | Indent(Int)             // indent increase (spaces)
  | Dedent(Int)             // indent decrease (spaces)
  | Newline                 // end of line
  | Eof                     // end of file