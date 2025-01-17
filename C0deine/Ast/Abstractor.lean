/- C0deine - Abstractor
   Converts the CST into the AST. If the source language is not C0, then
   programs that use features not in the source language are rejected here.
   - Thea Brick
 -/
import Batteries
import C0deine.Parser.Cst
import C0deine.Ast.Ast
import C0deine.Type.Typ
import C0deine.Context.Context
import C0deine.Context.Symbol
import C0deine.Config.Language
import C0deine.Utils.Comparison

/- Converts the concrete syntax tree to an abstract syntax tree
    as well as enforces a specific language level
 -/
namespace C0deine.Abstractor

def unsupported (lang : Language) (op : String) : Except String α :=
  throw s!"{lang} does not support {op}"

def Trans.type (lang : Language)
               (typ : Cst.Typ)
               : Except String (Option Ast.Typ) := do
  match typ with
  | .int => return some .int
  | .bool =>
    if lang.under .l2
    then unsupported lang "bools"
    else return some .bool
  | .char =>
    if lang.under .c0
    then unsupported lang "chars"
    else return some .char
  | .string =>
    if lang.under .c0
    then unsupported lang "strings"
    else return some .string
  | .void =>
    if lang.under .l3
    then unsupported lang "void function type"
    else return none
  | .tydef name =>
    if lang.under .l3
    then unsupported lang "aliased types"
    else return some (.tydef name)
  | .ptr tau =>
    if lang.under .l4
    then unsupported lang "pointer types"
    else
      let tau'Opt ← Trans.type lang tau
      match tau'Opt with
      | none =>
        if lang.under .c1
        then unsupported lang "void pointers"
        else panic "todo -- void pointers"
      | some tau' => return some (.ptr tau')
  | .arr tau =>
    if lang.under .l4
    then unsupported lang "array types"
    else
      let tau'Opt ← Trans.type lang tau
      match tau'Opt with
      | none => throw "Cannot have an array of void"
      | some tau' => return some (.arr tau')
  | .struct name =>
    if lang.under .l4
    then unsupported lang "structs"
    else
      return some (.struct name)

def Trans.type_nonvoid (lang : Language)
                       (typ : Cst.Typ)
                       : Except String Ast.Typ := do
  match ← Trans.type lang typ with
  | some typ' => return typ'
  | none => throw s!"void appeared where a type was expected"

def Trans.unop (lang : Language) (op : Cst.UnOp) : Except String Ast.UnOp := do
  match op with
  | .int .neg  => return .int .neg
  | .int .not  => return .int .not
  | .bool .not =>
    if lang.under .l2
    then unsupported lang "{op}"
    else return .bool .neg

def Trans.binop_int (lang : Language)
                    (op : Cst.BinOp.Int)
                    : Except String Ast.BinOp.Int := do
  let ret := fun res =>
    if lang.under .l2 then unsupported lang s!"{op}" else return res
  match op with
  | .plus  => return .plus
  | .minus => return .minus
  | .times => return .times
  | .div   => return .div
  | .mod   => return .mod
  | .and   => ret .and
  | .xor   => ret .xor
  | .or    => ret .or
  | .lsh   => ret .lsh
  | .rsh   => ret .rsh

def Trans.binop (lang : Language)
                (op : Cst.BinOp)
                : Except String Ast.BinOp := do
  match op with
  | .int i => Trans.binop_int lang i |>.map .int
  | .cmp c =>
    if lang.under .l2
    then unsupported lang s!"{c}"
    else
      match c with
      | .lt => return .cmp .less
      | .le => return .cmp .less_equal
      | .gt => return .cmp .greater
      | .ge => return .cmp .greater_equal
      | .eq => return .cmp .equal
      | .ne => return .cmp .not_equal
  | .bool b =>
    if lang.under .l2
    then unsupported lang s!"{b}"
    else
      match b with
      | .and => return .bool .and
      | .or  => return .bool .or

def Trans.asnop (lang : Language)
                (op : Cst.AsnOp)
                : Except String Ast.AsnOp := do
  match op with
  | .eq => return .eq
  | .aseq bop => Trans.binop_int lang bop |>.map .aseq

mutual
def Trans.expr (lang : Language) (e : Cst.Expr) : Except String Ast.Expr := do
  let ret := fun l' res =>
    if lang.under l' then unsupported lang s!"{e}" else return res
  match e with
  | .num n   => return .num n
  | .char c  => ret .c0 (.char c)
  | .str s   => ret .c0 (.str s)
  | .«true»  => ret .l2 .«true»
  | .«false» => ret .l2 .«false»
  | .null    => ret .l4 .null
  | .unop op e =>
    let op' ← Trans.unop lang op
    let e' ← Trans.expr lang e
    return .unop op' e'
  | .binop op l r =>
    let op' ← Trans.binop lang op
    let l' ← Trans.expr lang l
    let r' ← Trans.expr lang r
    return .binop op' l' r'
  | .ternop cond tt ff =>
    if lang.under .l2
    then unsupported lang "ternary operators"
    else
      let cond' ← Trans.expr lang cond
      let tt' ← Trans.expr lang tt
      let ff' ← Trans.expr lang ff
      return .ternop cond' tt' ff'
  | .app f args =>
    if lang.under .l3
    then unsupported lang "function application"
    else
      let args' ← Trans.exprs lang args
      return .app f args'
  | .alloc ty =>
    if lang.under .l4
    then unsupported lang "memory allocation"
    else
      let ty' ← Trans.type_nonvoid lang ty
      return .alloc ty'
  | .alloc_array ty e =>
    if lang.under .l4
    then unsupported lang "array allocation"
    else
      let ty' ← Trans.type_nonvoid lang ty
      let e' ← Trans.expr lang e
      return .alloc_array ty' e'
  | .var name => return .var name
  | .dot e field =>
    if lang.under .l4
    then unsupported lang "struct dot accessor"
    else
      let e' ← Trans.expr lang e
      return .dot e' field
  | .arrow e field =>
    if lang.under .l4
    then unsupported lang "struct arrow accessor"
    else
      let e' ← Trans.expr lang e
      return .arrow e' field
  | .deref e =>
    if lang.under .l4
    then unsupported lang "dereferencing"
    else
      let e' ← Trans.expr lang e
      return .deref e'
  | .index e indx =>
    if lang.under .l4
    then unsupported lang "array indexing"
    else
      let e' ← Trans.expr lang e
      let indx' ← Trans.expr lang indx
      return .index e' indx'
  | .result =>
    if lang.under .c0
    then unsupported lang "\\result"
    else return .result
  | .length e =>
    if lang.under .c0
    then unsupported lang "\\length"
    else
      let e' ← Trans.expr lang e
      return .length e'
termination_by sizeOf e

def Trans.exprs (lang : Language)
                (exps : List Cst.Expr)
                : Except String (List Ast.Expr) := do
  match exps with
  | [] => return []
  | e :: es =>
    let e' ← Trans.expr lang e
    let es' ← Trans.exprs lang es
    return e'::es'
termination_by sizeOf exps

end


def Trans.lvalue (lang : Language)
                 (lv : Cst.LValue)
                 : Except String Ast.LValue := do
  match lv with
  | .var name => return .var name
  | .dot lv field =>
    if lang.under .l4
    then unsupported lang "struct dot accessor"
    else
      let lv' ← Trans.lvalue lang lv
      return .dot lv' field
  | .arrow lv field =>
    if lang.under .l4
    then unsupported lang "struct arrow accessor"
    else
      let lv' ← Trans.lvalue lang lv
      return .arrow lv' field
  | .deref lv =>
    if lang.under .l4
    then unsupported lang "dereferencing"
    else
      let lv' ← Trans.lvalue lang lv
      return .deref lv'
  | .index lv indx =>
    if lang.under .l4
    then unsupported lang "array indexing"
    else
      let lv' ← Trans.lvalue lang lv
      let indx' ← Trans.expr lang indx
      return .index lv' indx'

def Trans.spec (lang : Language)
               (specs : List Cst.Spec)
               : Except String (List Ast.Anno) := do
  match specs with
  | []      => return []
  | s :: ss => do
    let s' ← (
        match s with
        | .requires e   => return .requires   (← Trans.expr lang e)
        | .ensures e    => return .ensures    (← Trans.expr lang e)
        | .loop_invar e => return .loop_invar (← Trans.expr lang e)
        | .assert e     => return .assert     (← Trans.expr lang e)
      )
    let ss' ← Trans.spec lang ss
    return s' :: ss'

def Trans.anno (lang : Language)
               (annos : List Cst.Anno)
               : Except String (List Ast.Anno) := do
  if lang.under .c0 then return [] else -- remove annotations if not c0
  match annos with
  | [] => return []
  | .line s  :: as
  | .block s :: as =>
    let s' ← Trans.spec lang s
    let as' ← Trans.anno lang as
    return s' ++ as'

def Trans.sepAnno (lang : Language)
                  (stmt : Cst.Stmt)
                  : Except String (List Ast.Anno × Cst.Stmt) := do
  match stmt with
  | .anno a s =>
    let a' ← Trans.anno lang [a]
    let (as', stmt') ← Trans.sepAnno lang s
    return (a' ++ as', stmt')
  | _ => return ([], stmt)

mutual
partial def Trans.stmts (lang : Language)
                (stmts : List Cst.Stmt)
                : Except String (List Ast.Stmt) := do
  match stmts with
  | [] => return []
  | .simp simp :: rest => Trans.simp lang simp rest
  | .ctrl ctrl :: rest => Trans.control lang ctrl rest
  | .block blk annos :: rest =>
    let blk' ← Trans.stmts lang blk
    let rest' ← Trans.stmts lang rest
    let annos' ← Trans.anno lang annos
    return blk' ++ (annos'.map .anno) ++ rest'
  | .anno a s :: rest =>
    let a' ← Trans.anno lang [a]
    let s' ← Trans.stmts lang (s :: rest)
    return a'.map .anno ++ s'

partial def Trans.simp (lang : Language)
               (simp : Cst.Simp)
               (rest : List Cst.Stmt)
               : Except String (List Ast.Stmt) := do
  match simp with
  | .assn lv op v =>
    let lv' ← Trans.lvalue lang lv
    let op' ← Trans.asnop lang op
    let v' ← Trans.expr lang v
    let rest' ← Trans.stmts lang rest
    return .assn lv' op' v' :: rest'
  | .post lv op =>
    if lang.under .l2
    then unsupported lang "post operators"
    else
      let lv' ← Trans.lvalue lang lv
      let rest' ← Trans.stmts lang rest
      match op with
      | .incr => return .assn lv' (.aseq .plus) (.num 1) :: rest'
      | .decr => return .assn lv' (.aseq .minus) (.num 1) :: rest'
  | .decl type name init =>
    let type' ← Trans.type_nonvoid lang type
    let init' ← init.mapM (Trans.expr lang)
    let rest' ← Trans.stmts lang rest
    return .decl type' name init' rest' :: []
  | .exp e =>
    if lang.under .l2
    then unsupported lang "expression statements"
    else
      let e' ← Trans.expr lang e
      let rest' ← Trans.stmts lang rest
      return .exp e' :: rest'

partial def Trans.control (lang : Language)
                  (ctrl : Cst.Control)
                  (rest : List Cst.Stmt)
                  : Except String (List Ast.Stmt) := do
  match ctrl with
  | .ite cond tt ff =>
    if lang.under .l2
    then unsupported lang "if-else statements"
    else
      let cond' ← Trans.expr lang cond
      let tt' ← Trans.stmts lang [tt]
      let ff' ← Trans.stmts lang [ff]
      let rest' ← Trans.stmts lang rest
      return .ite cond' tt' ff' :: rest'
  | .while cond body =>
    if lang.under .l2
    then unsupported lang "while loops"
    else
      let (annos', body) ← Trans.sepAnno lang body
      let cond' ← Trans.expr lang cond
      let body' ← Trans.stmts lang [body]
      let rest' ← Trans.stmts lang rest
      return .while cond' annos' body' :: rest'
  | .«for» initOpt cond stepOpt body =>
    if lang.under .l2
    then unsupported lang "for loops"
    else
      match stepOpt with
      | some (.decl _ _ _) => throw s!"For loop steps cannot have declarations"
      | some step =>
        let bodyStep := .block [body, .simp step] []
        let whileStmt := .ctrl (.while cond bodyStep)
        match initOpt with
        | some init =>
          let initBody' ← Trans.simp lang init [whileStmt]
          let rest' ← Trans.stmts lang rest
          return initBody'.append rest'
        | none => Trans.stmts lang (whileStmt :: rest)
      | none =>
        let whileStmt := .ctrl (.while cond body)
        match initOpt with
        | some init =>
          let initBody' ← Trans.simp lang init [whileStmt]
          let rest' ← Trans.stmts lang rest
          return initBody'.append rest'
        | none => Trans.stmts lang (whileStmt :: rest)
  | .return (some e) =>
    let e' ← Trans.expr lang e
    let rest' ← Trans.stmts lang rest
    return .return (some e') :: rest'
  | .return (none) =>
    if lang.under .l3
    then unsupported lang "void return statements"
    else
      let rest' ← Trans.stmts lang rest
      return .return none :: rest'
  | .assert e =>
    if lang.under .l3
    then unsupported lang "assert statements"
    else
      let e' ← Trans.expr lang e
      let rest' ← Trans.stmts lang rest
      return .assert e' :: rest'
  | .error e =>
    if lang.under .c0
    then unsupported lang "error statements"
    else
      let e' ← Trans.expr lang e
      let rest' ← Trans.stmts lang rest
      return .error e' :: rest'
end

def Trans.param (lang : Language)
                (p : Cst.Param)
                : Except String Ast.Param := do
  if lang.under .l3
  then unsupported lang "functions with parameters"
  else return ⟨← Trans.type_nonvoid lang p.type, p.name⟩

def Trans.field (lang : Language)
                (f : Cst.Field)
                : Except String Ast.Field := do
  return ⟨← Trans.type_nonvoid lang f.type, f.name⟩

def Trans.gdecl (lang : Language)
                (gdec : Cst.GDecl)
                : Except String (Ast.GDecl) := do
  match gdec with
  | .fdecl f =>
    if lang.under .l3
    then unsupported lang "function declarations"
    else
      let type' ← Trans.type lang f.type
      let params' ← f.params.mapM (Trans.param lang)
      let annos' ← Trans.anno lang f.annos
      return .fdecl ⟨type', f.name, params', annos'⟩
  | .fdef f =>
    if f.name.name != "main" && lang.under .l3
    then unsupported lang "functions that aren't main"
    else
      let type' ← Trans.type lang f.type
      let params' ← f.params.mapM (Trans.param lang)
      let annos' ← Trans.anno lang f.annos
      let body' ← Trans.stmts lang [f.body]
      return .fdef ⟨⟨type', f.name, params', annos'⟩, body'⟩
  | .tydef t =>
    if lang.under .l3
    then unsupported lang "typdefs"
    else return .tydef ⟨← Trans.type_nonvoid lang t.type, t.name⟩
  | .sdecl s =>
    if lang.under .l4
    then unsupported lang "struct declarations"
    else return .sdecl ⟨s.name⟩
  | .sdef  s =>
    if lang.under .l4
    then unsupported lang "struct definitions"
    else return .sdef ⟨s.name, ← s.fields.mapM (Trans.field lang)⟩
  | .cdir _d =>
    throw "Compiler directives must appear at the beginning of a file."

def abstract (lang : Language)
             (header : Option Cst.Prog)
             (prog : Cst.Prog)
             : Except String Ast.Prog := do
  let hast ←
    match header with
    | none | some [] => pure []
    | some hcst =>
      if lang.under .l3
      then unsupported lang "header files"
      else hcst.mapM (Trans.gdecl lang)
  let ast ← prog.mapM (Trans.gdecl lang)
  return ⟨hast, ast⟩
