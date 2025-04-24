// src/syntax/ast/ast.gleam

// AST definitions for the Liria DSL front-end.
// Keeps the core minimal: scope, state, react blocks, expressions, and inner statements.

pub type Program =
  List(Statement)

// ── Top-level statements ────────────────────────────────────
pub type Statement {
  ScopeStmt(String, List(Statement))
  StateStmt(String, Expr)
  ReactStmt(String, List(InnerStmt))
}

// ── Expressions ─────────────────────────────────────────────
pub type Expr {
  IntExpr(Int)
  IdentExpr(String)
  BinaryExpr(Expr, String, Expr)
  CallExpr(String, List(Expr))
}

// ── Inner statements ────────────────────────────────────────
pub type InnerStmt {
  DoStmt
  EmitStmt(String, Option(Expr))
  LoopStmt(Expr)
}