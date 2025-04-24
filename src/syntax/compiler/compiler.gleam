// src/syntax/compiler/compiler.gleam

// Compiler: transform Liria AST into Julia source that uses Itera API.

import gleam/string.{ concat, join, append }
import gleam/list
import gleam/int.{ to_string }

import syntax/ast.{ Program, Statement, Expr, InnerStmt }

// ── Entry point ───────────────────────────────────────────────────────────────

pub fn compile_to_julia_module(module_base: String, program: Program) -> String {
  let module_name = append(module_base, "Rule")

  let defs = list.map(statement_to_definition, program)
  let body_blocks = list.map(definition_to_code, defs)
  let body = join("\n\n", body_blocks)

  let header = concat(["module ", module_name, "\nusing Itera\n\n"])
  let footer = concat(["\nend  # module ", module_name])

  concat([header, body, footer])
}

// ── Intermediate structure ────────────────────────────────────────────────────

pub type CompileDefinition {
  StateDefinition(String, Expr)
  ReactDefinition(String, List(InnerStmt))
}

// Map top-level Statement to CompileDefinition
fn statement_to_definition(stmt: Statement) -> CompileDefinition {
  case stmt {
    StateStmt(name, expr) -> StateDefinition(name, expr)
    ReactStmt(event, body) -> ReactDefinition(event, body)
    ScopeStmt(_, _) -> StateDefinition("skip", IntExpr(0)) // TODO: support scopes
  }
}

// ── Top-level code generation ────────────────────────────────────────────────

fn definition_to_code(def: CompileDefinition) -> String {
  case def {
    StateDefinition(name, expr) ->
      concat(["game.data[:", name, "] = ", expr_to_src(expr)])

    ReactDefinition(event, body) -> {
      let steps_src = body
        |> list.map(inner_to_step)
        |> join(", ")
      let flow_line = concat(["let flow = Pipeline.Flow([", steps_src, "])"])
      let handler = concat([
        "Effect.add!(game, Effect.Event(:", event,
        ", :on_", event,
        ", (s,...) -> Pipeline.flow_execute!(flow, s, s.rng)))"
      ])
      concat([flow_line, "\n", handler])
    }
  }
}

// ── InnerStmt → Pipeline.Step source ─────────────────────────────────────────

fn inner_to_step(st: InnerStmt) -> String {
  case st {
    DoStmt ->
      "Pipeline.Step(:do; operation=(s,r)->nothing)"

    EmitStmt(target, maybe_count) -> {
      let op_src = concat(["(s,r)->Effect.event_emit!(s, :",
        target, ")"])
      let rep_src = case maybe_count {
        Some(e) -> expr_to_src(e)
        None -> "1"
      }
      concat([
        "Pipeline.Step(:emit; operation=", op_src,
        ", repetition=", rep_src, ")"
      ])
    }

    LoopStmt(times_expr) -> {
      let rep_src = expr_to_src(times_expr)
      concat([
        "Pipeline.Step(:loop; operation=(s,r)->nothing, repetition=",
        rep_src, ")"
      ])
    }
  }
}

// ── Expr → Julia source ──────────────────────────────────────────────────────

fn expr_to_src(e: Expr) -> String {
  case e {
    IntExpr(i) -> to_string(i)
    IdentExpr(id) -> id
    BinaryExpr(l, op, r) ->
      concat(["(", expr_to_src(l), " ", op, " ", expr_to_src(r), ")"])
    CallExpr(fn_name, args) ->
      concat([
        fn_name, "(", join(\", \", list.map(expr_to_src, args)), ")"
      ])
  }
}