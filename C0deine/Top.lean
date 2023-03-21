import C0deine
import Cli

namespace C0deine.Top

def version := "0.0.1"

open Cli

def runTopCmd (p : Parsed) : IO UInt32 := do
  if !p.hasPositionalArg "input" then
    panic! "Missing file argument"
  let input : System.FilePath :=
    p.positionalArg! "input" |>.as! String
  let libInput : Option System.FilePath :=
    p.flag? "library" |>.map (·.as! String)
  let tcOnly : Bool := p.hasFlag "typecheck"
  let verbose : Bool := p.hasFlag "verbose"

  if !(← input.pathExists) then
    panic! "Input file does not exist: {input}"
  if ← input.isDir then
    panic! "Input path is a directory: {input}"

  if libInput.isSome then
    if !(← libInput.get!.pathExists) then
      panic! "Header file does not exist: {libInput}"
    if ← libInput.get!.isDir then
      panic! "Header path is a directory: {libInput}"

  let lang : Language :=
    match input.extension with
    | none => panic! "File is missing extension"
    | some l =>
      match Language.fromString l with
      | .some lang => lang
      | .none => panic! s!"Unrecognized extension/language {l}"

  let contents ← IO.FS.readFile input
  let header ← libInput.mapM (IO.FS.readFile)

  if verbose then IO.println "parsing header"

  let (header, headerTydefs, ctx) ← do
    match header with
    | none =>
      pure (none, .empty, .new)
    | some h =>
    match Parser.C0Parser.prog.run h.toUTF8 .new with
    | ((.error e, state), _) =>
      IO.println s!"{e () |>.formatPretty state}"
      return 1
    | ((.ok (cst, tydefs), _), ctx) =>
      pure (some cst, tydefs, ctx)

  if verbose then IO.println "parsing input"

  match (Parser.C0Parser.prog headerTydefs).run contents.toUTF8 ctx with
  | ((.error e, state), _) =>
    IO.println s!"{e () |>.formatPretty state}"
    return 1
  | ((.ok (cst,_), _), ctx) =>

  -- if verbose then IO.println cst
  if verbose then IO.println "abstracting"

  let ast ← IO.ofExcept <| Abstractor.abstract lang cst

  if verbose then IO.println ast
  if verbose then IO.println "typechecking"

  match Typechecker.typecheck ast with
  | .error e =>
    IO.println s!"\n{e}\n"
    return 1
  | .ok tst =>

  if tcOnly then return 0

  IO.println "typechecked!"
  IO.println tst

  return 0

def topCmd : Cmd := `[Cli|
  c0deine VIA runTopCmd; [version]
  "a compiler for C0."

  FLAGS:
    t, typecheck;               "Only typecheck a file"
    v, verbose;                 "Give verbose output"
    e, emit : String;           "Specify compilation target (either x86 or wasm)"
    l, library : String;        "Specify header file"
    O0;                         "Compile with no optimizations"
    O1;                         "Compile with optimizations"


  ARGS:
    input : String;      "The input file"
]
