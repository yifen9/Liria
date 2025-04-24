// src/main.gleam

// CLI entry: compile .liria scripts to Julia modules in build/
// If no arguments are provided it compiles every .liria file under "rule/".

import gleam/argv.{ argv }
import gleam/io.{ read_file, write_file, println, eprintln }
import gleam/os.{ list_dir }
import gleam/list.{ filter, map, last }
import gleam/string.{ ends_with, strip_suffix, split, capitalize, concat }

import syntax/lexer/tokenizer.{ text_lex }
import syntax/parser/grammar.{ parser_program }
import syntax/compiler.{ compile_to_julia_module }

// Entry point
pub fn run() -> Nil {
  let args = argv()

  // Build the list of scripts to compile
  let scripts = case args {
    [] ->
      list_dir("rule")
      |> filter(fn(name) { ends_with(name, ".liria") })
      |> map(fn(name) { concat(["rule/", name]) })

    [single] -> [single]

    _ -> {
      eprintln("Usage: liria [script.liria] or no arguments to compile all in rule/")
      []
    }
  }

  // Compile each script
  for script in scripts {
    compile_script(script)
  }
}

// Compile one .liria script into build/<Base>Rule.jl
fn compile_script(path: String) -> Nil {
  case read_file(path) {
    Ok(source) ->
      case text_lex(source) |> parser_program {
        Some(#(ast, _)) -> {
          // Derive module base name
          let segments = split(path, "/")
          let file_name = case last(segments) {
            Some(name) -> name
            None -> path
          }
          let base_raw  = strip_suffix(file_name, ".liria")
          let base_name = capitalize(base_raw)

          // Generate Julia code and write to file
          let julia_code = compile_to_julia_module(base_name, ast)
          let out_path   = concat(["build/", base_name, "Rule.jl"])
          case write_file(out_path, julia_code) {
            Ok(_) -> println(concat(["Written: ", out_path]))
            Error(e) -> eprintln(concat(["Write error: ", e]))
          }
        }
        None -> eprintln(concat(["Parse error: ", path]))
      }

    Error(e) -> eprintln(concat(["Read error: ", path, " ", e]))
  }
}