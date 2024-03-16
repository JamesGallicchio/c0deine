/- C0deine - TST.Dynamics
   An encoding of the dynamic semantics of C0 programs (in the TST). Importantly
   in our current model, contracts are not ran, so there is no rules defining
   their execution.

   Hopefully using this, we will be able to reason about C0 code, and maybe
   write tactics to do so, similar to the Pauline project.

   - Thea Brick
 -/
import C0deine.AuxDefs
import C0deine.Type.Tst

namespace C0deine.Tst.Dynamics

open Numbers Tst

/- TODO: should be abstracted for the various dynamics -/
inductive Exception
| memory
| arithmetic
| abort
| error (msg : String)

inductive Address
| ref : Nat → Address
| null : Address
deriving Inhabited, Repr

def Address.toNat : Address → Nat
  | .ref n => n
  | .null  => 0

def Address.toInt32 : Address → Int32 := Signed.ofNat ∘ Address.toNat

-- should this take in a type/be enforced?
inductive Value
| num  : Int32 → Value
| char : Char → Value
| str  : String → Value
| «true» | «false»
| nothing
| addr : Address → Value
| struct : (Symbol → Value) → Value
| arr : List Value → Value
deriving Inhabited

inductive BinOp
| int (op : BinOp.Int)
| cmp (op : Comparator)

open Typ.Notation in
inductive TypeValue : Value → Typ → Prop
| num     : TypeValue (.num  i) (int)
| char    : TypeValue (.char c) (char)
| string  : TypeValue (.str  s) (string)
| «true»  : TypeValue .true     (bool)
| «false» : TypeValue .false    (bool)
| ptr     : TypeValue (.addr a) (ty *) -- todo is this right?
| arr     : TypeValue (.addr a) (τ[])  --      should check heap?
-- todo finish

open Typ.Notation in
inductive Default : Typ → Value → Prop
| int    : Default (int) (.num 0)
| bool   : Default (bool) .false
| ptr    : Default (t *) (.addr .null)
| struct : Default (struct t) (.addr .null)
| arr    : Default (t[]) (.addr .null)

-- inductive IsExtern
--   : (Δ : ProgTc (p : Prog))
--   → (type : Option Typ)
--   → (f : Ident)
--   → (params : List Param)
--   → Prop
-- where
-- | extern : (GDecl.fdecl ⟨type, f, params, annos⟩) ∈ Prog.program p
--          → IsExtern Δ type f params

-- inductive FindFDef
--   : (Δ : ProgTc (p : Prog))
--   → (type : Option Typ)
--   → (f : Ident)
--   → (params : List Param)
--   → (body : List Stmt)
--   → Prop
-- where
-- | body : (GDecl.fdef ⟨⟨type, f, params, annos⟩, body⟩) ∈ Prog.program p
--        → FindFDef Δ type f params body

/- Continuation frames can result in a value or an address
    importantly, addresses are just the intermediate results, they aren't
    the same as addresses computed by `alloc`, etc.
-/
inductive Cont.Res | val | addr

/- merge Cont and ContFrame from the lecture notes bc of addressing modalities
-/
-- todo: should there be more type enforcement here?
variable (Δ : GCtx) (Γ : FCtx) in
open Typ.Notation in
inductive Cont : Cont.Res → Type
| nil : Cont .val                                              -- ·
| unop : UnOp → Cont .val → Cont .val                          -- op _
| binop_int₁                                                   -- _ ⊕ e
  : {τ : {τ : Typ // τ = (int)}}
  → BinOp.Int → Expr Δ Γ τ → Cont .val → Cont .val
| binop_eq₁
  : Comparator → Expr Δ Γ τ → Cont .val → Cont .val
| binop_rel_int₁
  : {τ : {τ : Typ // τ = (int)}}
  → Comparator → Expr Δ Γ τ → Cont .val → Cont .val
| binop_rel_char₁
  : {τ : {τ : Typ // τ = (char)}}
  → Comparator → Expr Δ Γ τ → Cont .val → Cont .val
| binop_int₂                                                   -- c ⊕ _
  : Value → BinOp.Int → Cont .val → Cont .val
| binop_eq₂
  : Value → Comparator → Cont .val → Cont .val
| binop_rel_int₂
  : Value → Comparator → Cont .val → Cont .val
| binop_rel_char₂
  : Value → Comparator → Cont .val → Cont .val
| and                                                          -- _ && e
  : {τ : {τ : Typ // τ = (bool)}}
  → Expr Δ Γ τ → Cont .val → Cont .val
| or                                                           -- _ || e
  : {τ : {τ : Typ // τ = (bool)}}
  → Expr Δ Γ τ → Cont .val → Cont .val
| ternop                                                       -- _ ? e₁ : e₂
  : {τ : {τ : Typ // τ = (bool)}}
  → Expr Δ Γ τ → Expr Δ Γ τ' → Cont .val → Cont .val
-- | app                                                       -- f(vs,_,es)
  -- : Ident → List Value → List Expr → Cont .val → Cont .val
| alloc_arr  : Typ → Cont .val → Cont .val                     -- alloc_array(τ,_)
| dot        : Symbol → Cont .addr → Cont .addr                -- &(_.f)
| deref      : Cont .val → Cont .addr                          -- *_
| index₁                                                       -- &(_[e])
  : {τ : {τ' : Typ // τ' = (int)}}
  → Expr Δ Γ τ → Cont .addr → Cont .val
| index₂     : Address → Cont .addr → Cont .val                -- &(a[_])
| stmt       : Stmt Δ Γ ρ → Cont .val → Cont .val              -- s
| assn₁                                                        -- assn(_, e)
  : Expr Δ Γ τ → Cont .val → Cont .addr
| assn₂      : Address → Cont .val → Cont .val                 -- assn(a, _)
| assn_var   : Symbol → Cont .val → Cont .val                  -- assn(x, _)
| ite                                                          -- if(_, s₁, s₂)
  : List (Stmt Δ Γ ρ) → List (Stmt Δ Γ ρ) → Cont .val → Cont .val
| «while»                                                      -- while(_){...}
  : List (Stmt Δ Γ ρ) → Cont .val → Cont .val
| «return»   : Cont .val                                       -- return _
| assert     : Cont .val → Cont .val                           -- assert(_)
| error      : Cont .val → Cont .val                           -- error(_)
| discard    : Cont .val → Cont .val                           -- discard

def Cont.consStmtList (K : Cont Δ Γ .val) : List (Stmt Δ Γ ρ) → Cont Δ Γ .val
  | [] => K
  | s :: stmts => .stmt s (consStmtList K stmts)

inductive DynResult : Prop
| val      : Value → Cont Δ Γ r → DynResult
| eval     : Expr Δ Γ τ → Cont Δ Γ r → DynResult
| exec     : Stmt Δ Γ ρ → Cont Δ Γ r → DynResult
| exec_seq : List (Stmt Δ Γ ρ) → Cont Δ Γ r → DynResult
| exn      : Exception → DynResult
| nop      : Cont Δ Γ r → DynResult       -- maybe move into AST
| res      : Int32 → DynResult


def Environment := Symbol → Option Value

namespace Environment

def empty : Environment := fun _ => .none
def update (η : Environment) (x : Symbol) (v : Value) : Environment :=
  Function.update η x v

def find! (η : Environment) (x : Symbol) : Value :=
  match η x with
  | .none   => panic! s!"var not found"
  | .some v => v

def ofLists (params : List (Typ.Typed Symbol))
            (vargs : List Value)
            : Environment :=
  List.zip params vargs
  |>.foldl (fun η (p, v) => η.update p.data v) Environment.empty

end Environment

structure StackFrame where
  environment : Environment
  continuation : Cont Δ Γ .val

structure Heap where
  data : Nat → Option Value
  next : Nat

namespace Heap

def empty : Heap := { data := fun _ => .none, next := 0 }
def update (H : Heap) (a : Nat) (v : Value) : Heap :=
  { data := Function.update H.data a v, next := H.next }

def find (H : Heap) : Address → Value ⊕ Exception
  | .null => .inr .memory
  | .ref a =>
    match H.data a with
    | .none => .inr .memory
    | .some v => .inl v

def add (H : Heap) (v : Value) : Address × Heap :=
  (.ref H.next, ⟨fun a => if a = H.next then v else H.data a, H.next + 1⟩)

end Heap

inductive Step.UnOp : UnOp → Value → Value → Prop
| int_neg : UnOp (.int .neg)  (.num c) (.num (-c))
| int_not : UnOp (.int .not)  (.num c) (.num c.not)
| neg_t   : UnOp (.bool .neg) (.true)  (.false)
| neg_f   : UnOp (.bool .neg) (.false) (.true)


def Step.ofNum : Int32 → Value ⊕ Exception := .inl ∘ .num

def Step.ofNum_exn : Option Int32 → Value ⊕ Exception
  | .none => .inr .arithmetic
  | .some i => .inl (.num i)

inductive Step.BinOp.Int : Value → BinOp.Int → Value → Value ⊕ Exception → Prop
| add : BinOp.Int (.num c₁) (.plus)  (.num c₂) (ofNum     (c₁ + c₂))
| sub : BinOp.Int (.num c₁) (.minus) (.num c₂) (ofNum     (c₁ - c₂))
| mul : BinOp.Int (.num c₁) (.times) (.num c₂) (ofNum     (c₁ * c₂))
| div : BinOp.Int (.num c₁) (.div)   (.num c₂) (ofNum_exn (c₁ / c₂))
| mod : BinOp.Int (.num c₁) (.mod)   (.num c₂) (ofNum_exn (c₁ % c₂))
| and : BinOp.Int (.num c₁) (.and)   (.num c₂) (ofNum     (c₁ &&& c₂))
| xor : BinOp.Int (.num c₁) (.xor)   (.num c₂) (ofNum     (c₁ ^^^ c₂))
| or  : BinOp.Int (.num c₁) (.or)    (.num c₂) (ofNum     (c₁ ||| c₂))
| lsh : BinOp.Int (.num c₁) (.lsh)   (.num c₂) (ofNum     (c₁ <<< c₂))
| rsh : BinOp.Int (.num c₁) (.rsh)   (.num c₂) (ofNum     (c₁ >>> c₂))


def Step.ofBool : Bool → Value
  | true  => .true
  | false => .false

inductive Step.BinOp.Cmp : Value → Comparator → Value → Value → Prop
| lt  : BinOp.Cmp (.num c₁) (.less         ) (.num c₂) (ofBool (c₁ < c₂))
| gt  : BinOp.Cmp (.num c₁) (.greater      ) (.num c₂) (ofBool (c₁ > c₂))
| eq  : BinOp.Cmp (.num c₁) (.equal        ) (.num c₂) (ofBool (c₁ = c₂))
| ne  : BinOp.Cmp (.num c₁) (.not_equal    ) (.num c₂) (ofBool (c₁ ≠ c₂))
| le  : BinOp.Cmp (.num c₁) (.less_equal   ) (.num c₂) (ofBool (c₁ ≤ c₂))
| ge  : BinOp.Cmp (.num c₁) (.greater_equal) (.num c₂) (ofBool (c₁ ≥ c₂))


structure State (p : Prog) where
  H : Heap
  S : List StackFrame
  η : Environment
  r : DynResult

-- local notation:50 Δ:51 " ; " H:51 " ; " S:51 " ; " η:51 " |= " r:51 =>
  -- State.mk Δ H S η r
local notation:50 H:51 " ; " S:51 " ; " η:51 " |= " r:51 =>
  State.mk H S η r

inductive Step : State p → State p → Prop
| num
  : Step (H; S; η |= (.eval (.num  c) K))
         (H; S; η |= (.val  (.num  c) K))
| char
  : Step (H; S; η |= (.eval (.char c) K))
         (H; S; η |= (.val  (.char c) K))
| str
  : Step (H; S; η |= (.eval (.str  s) K))
         (H; S; η |= (.val  (.str  s) K))
| «true»
  : Step (H; S; η |= (.eval .true K))
         (H; S; η |= (.val  .true K))
| «false»
  : Step (H; S; η |= (.eval .false K))
         (H; S; η |= (.val  .false K))
| null
  : Step (H; S; η |= (.eval .null         K))
         (H; S; η |= (.val  (.addr .null) K))
| unop
  : Step (H; S; η |= (.eval (.unop op h e) K))
         (H; S; η |= (.eval e (.unop op K)))
| unop_res
  : Step.UnOp op c v
  → Step (H; S; η |= (.val c (.unop op K)))
         (H; S; η |= (.val v K))
| binop_int₁
  : Step (H; S; η |= (.eval (.binop_int op e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.binop_int₁ op e₂ K)))
| binop_int₂
  : Step (H; S; η |= (.val c₁ (.binop_int₁ op e₂ K)))
         (H; S; η |= (.eval e₂ (.binop_int₂ c₁ op K)))
| binop_int
  : Step.BinOp.Int c₁ op c₂ (.inl v)
  → Step (H; S; η |= (.val c₂ (.binop_int₂ c₁ op K)))
         (H; S; η |= (.val v K))
| binop_int_exn
  : Step.BinOp.Int c₁ op c₂ (.inr exn)
  → Step (H; S; η |=  (.val c₂ (.binop_int₂ c₁ op K)))
         (H; S; η |=  (.exn exn))
| binop_eq₁
  : Step (H; S; η |= (.eval (.binop_eq op h₁ e₁ e₂ h₂ h₃) K))
         (H; S; η |= (.eval e₁ (.binop_eq₁ op e₂ K)))
| binop_eq₂
  : Step (H; S; η |= (.val c₁ (.binop_eq₁ op e₂ K)))
         (H; S; η |= (.eval e₂ (.binop_eq₂ c₁ op K)))
| binop_eq
  : Step.BinOp.Cmp c₁ op c₂ v
  → Step (H; S; η |= (.val c₂ (.binop_eq₂ c₁ op K)))
         (H; S; η |= (.val v K))
| binop_rel_int₁
  : Step (H; S; η |= (.eval (.binop_rel₁ op h e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.binop_rel_int₁ op e₂ K)))
| binop_rel_int₂
  : Step (H; S; η |= (.val c₁ (.binop_rel_int₁ op e₂ K)))
         (H; S; η |= (.eval e₂ (.binop_rel_int₂ c₁ op K)))
| binop_rel_int
  : Step.BinOp.Cmp c₁ op c₂ v
  → Step (H; S; η |= (.val c₂ (.binop_rel_int₂ c₁ op K)))
         (H; S; η |= (.val v K))
| binop_rel_char₁
  : Step (H; S; η |= (.eval (.binop_rel₂ op h e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.binop_rel_char₁ op e₂ K)))
| binop_rel_char₂
  : Step (H; S; η |= (.val c₁ (.binop_rel_char₁ op e₂ K)))
         (H; S; η |= (.eval e₂ (.binop_rel_char₂ c₁ op K)))
| binop_rel_char
  : Step.BinOp.Cmp c₁ op c₂ v
  → Step (H; S; η |= (.val c₂ (.binop_rel_char₂ c₁ op K)))
         (H; S; η |= (.val v K))
| and₁
  : Step (H; S; η |= (.eval (.binop_bool .and e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.and e₂ K)))
| and₂
  : Step (H; S; η |= (.val .true (.and e₂ K)))
         (H; S; η |= (.eval e₂ K))
| and_sc
  : Step (H; S; η |= (.val .false (.and e₂ K)))
         (H; S; η |= (.val .false K))
| or₁
  : Step (H; S; η |= (.eval (.binop_bool .or e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.or e₂ K)))
| or₂
  : Step (H; S; η |= (.val .false (.or e₂ K)))
         (H; S; η |= (.eval e₂ K))
| or_sc
  : Step (H; S; η |= (.val .true (.or e₂ K)))
         (H; S; η |= (.val .true K))
| ternop
  : Step (H; S; η |= (.eval (.ternop cc tt ff h) K))
         (H; S; η |= (.eval cc (.ternop tt ff K)))
| ternop_t
  : Step (H; S; η |= (.val .true (.ternop tt ff K)))
         (H; S; η |= (.eval tt K))
| ternop_f
  : Step (H; S; η |= (.val .false (.ternop tt ff K)))
         (H; S; η |= (.eval ff K))
-- todo generalise this a bit : )
/-
| app_args
  : Step (H; S; η |= (.eval (.app f h₁ τs eq args) K))
         (H; S; η |= (.eval e (.app f [] [] K)))
| app_args_cont
  : Step (H; S; η |= (.val v (.app f vargs (e::args) K)))
         (H; S; η |= (.eval e (.app f (vargs ++ [v]) args K)))
| app_args_call
  : FindFDef Δ τ_opt f ps body
  → Step (H; S; η |= (.val v (.app f vargs [] K)))
         (H; (⟨η, K⟩::S); (Environment.ofLists ps vargs) |= (.exec_seq body .nil))
| app_args_extern_nonvoid
  : IsExtern Δ (.some ty) f params
  → TypResolves Δ.prog_ctx ty τ
  → (H' : Heap)
  → TypeValue res τ
  → Step (H ; S; η |= (.val v (.app f vargs [] K)))
         (H'; S; η |= (.val res K))
| app_args_extern_void
  : IsExtern Δ .none f params
  → (H' : Heap)
  → Step (H ; S; η |= (.val v (.app f vargs [] K)))
         (H'; S; η |= (.nop K))
| app_unit_extern_nonvoid
  : IsExtern Δ (.some ty) f params
  → TypResolves Δ.prog_ctx ty τ
  → (H' : Heap)
  → TypeValue res τ
  → Step (H ; S; η |= (.eval (.app f []) K))
         (H'; S; η |= (.val res K))
| app_unit_extern_void
  : IsExtern Δ .none f params
  → (H' : Heap)
  → Step (H ; S; η |= (.eval (.app f []) K))
         (H'; S; η |= (.nop K))
| app_unit_call
  : FindFDef Δ τ_opt f params body
  → Step (H; S; η |= (.eval (.app f []) K))
         (H; (⟨η, K⟩ :: S); (Environment.empty) |= (.exec_seq body .nil))
-/
-- todo app
| alloc
  : Default τ v
  → H.add v = (a, H')
  → Step (H ; S; η |= (.eval (.alloc τ) K))
         (H'; S; η |= (.val (.addr a) K))
| alloc_array
  : Step (H; S; η |= (.eval (.alloc_array τ e) K))
         (H; S; η |= (.eval e (.alloc_arr τ K)))
| alloc_array_lt_zero
  : n < 0
  → Step (H; S; η |= (.val (.num n) (.alloc_arr τ K)))
         (H; S; η |= (.exn .memory))
| alloc_array_val
  : n ≥ 0
  → Default τ v
  → H.add (.arr (List.ofFn (n := n.toNat) (fun _ => v))) = (a, H')
  → Step (H ; S; η |= (.val (.num n) (.alloc_arr τ K)))
         (H'; S; η |= (.val (.addr a) K))
| var
  : Step (H; S; η |= (.eval (.var x h) K))
         (H; S; η |= (.val (η.find! x) K))
| dot
  : Step (H; S; η |= (.eval (.dot e f h₁ h₂) K))
         (H; S; η |= (.eval e (.dot f K)))
| dot_val
  : Step (H; S; η |= (.val (.struct fields) (.dot f K)))
         (H; S; η |= (.val (fields f) K))
| dot_null
  : Step (H; S; η |= (.val (.addr .null) (.dot f K)))
         (H; S; η |= (.exn .memory))
| deref₁
  : Step (H; S; η |= (.eval (.deref e) K))
         (H; S; η |= (.eval e (.deref K)))
| deref_val
  : H.find a = .inl v
  → Step (H; S; η |= (.val (.addr a) (.deref K)))
         (H; S; η |= (.val v K))
| deref_exn
  : H.find a = .inr exn
  → Step (H; S; η |= (.val (.addr a) (.deref K)))
         (H; S; η |= (.exn exn))
| index₁
  : Step (H; S; η |= (.eval (.index e₁ e₂) K))
         (H; S; η |= (.eval e₁ (.index₁ e₂ K)))
| index₂
  : Step (H; S; η |= (.val (.addr a) (.index₁ e₂ K)))
         (H; S; η |= (.eval e₂ (.index₂ a K)))
| index_val
  : H.find a = .inl (.arr arr)
  → 0 ≤ i
  → i.toNat < arr.length
  → Step (H; S; η |= (.val (.num i) (.index₂ a K)))
         (H; S; η |= (.val (arr.get! i.toNat) K))
| index_lt_zero
  : H.find a = .inl (.arr arr)
  → i < 0
  → Step (H; S; η |= (.val (.num i) (.index₂ a K)))
         (H; S; η |= (.exn .memory))
| index_gt_length
  : H.find a = .inl (.arr arr)
  → i.toNat ≥ arr.length
  → Step (H; S; η |= (.val (.num i) (.index₂ a K)))
         (H; S; η |= (.exn .memory))
| index_null
  : H.find a = .inr exn
  → Step (H; S; η |= (.val i (.index₂ a K)))
         (H; S; η |= (.exn exn))
/- Result/Length not implemented here since we don't not execute that code
    in our current model. -/
/- STATEMENTS -/
| decl
  : Step (H; S; η |= (.exec (.decl ⟨τ, x⟩ h body) K))
         (H; S; (η.update x .nothing) |= (.exec_seq body.toList K))
| decl_assn
  : Step (H; S; η |= (.exec (.decl_init ⟨τ, x⟩ e h₁ h₂ body) K))
         (H; S; (η.update x .nothing) |= (.eval e.val (K.consStmtList body.toList)))
| assn_var_eq₁
  : Step (H; S; η |= (.exec (.assign_var (.var x hl) h₁ e h₂) K))
         (H; S; η |= (.eval e.val (.assn_var x K)))
| assn_var_eq₂
  : Step (H; S; η |= (.val v (.assn_var x K)))
         (H; S; (η.update x v) |= (.nop K))
| assn_addr_eq₁
  : Step (H; S; η |= (.exec (.assign lv h₁ e h₂) K))
         (H; S; (η.update x v) |= (.eval lv.toExpr (.assn₁ e.val K)))
| assn_addr_eq₂
  : Step (H; S; η |= (.val (.addr a) (.assn₁ e K)))
         (H; S; η |= (.eval e (.assn₂ a K)))
| assn_addr_eq_val
  : Step (H; S; η |= (.val v (.assn₂ (.ref a) K)))
         ((H.update a v); S; η |= (.nop K))
| assn_addr_null
  : Step (H; S; η |= (.val v (.assn₂ .null K)))
         (H; S; η |= (.exn .memory))
| assn_var_op
  : Step (H; S; η |= (.exec (.asnop (.var x h) op e) K))
         (H; S; η |= (.eval (.var x h) (.binop_int₁ op e.val (.assn_var x K))))
| assn_addr_op_val                    -- todo: double check this probs
  : H.find a = .inl (.num da)
  → Step (H; S; η |= (.val (.num c) (.assn₂ a K)))
         (H; S; η |= (.eval (.binop_int op (Expr.intType (.num da) (by rfl))
                                           (Expr.intType (.num c) (by rfl))) K))
| assn_addr_op_exn                    -- todo: likewise, double check
  : H.find a = .inr exn
  → Step (H; S; η |= (.val (.num c) (.assn₂ a K)))
         (H; S; η |= (.exn exn))
| exp₁
  : Step (H; S; η |= (.exec (.expr e) K))
         (H; S; η |= (.eval e.val (.discard K)))
| exp₂
  : Step (H; S; η |= (.val v (.discard K)))
         (H; S; η |= (.nop K))
| ite
  : Step (H; S; η |= (.exec (.ite e tt ff) K))
         (H; S; η |= (.eval e.val (.ite tt ff.toList K)))
| ite_t
  : Step (H; S; η |= (.val .true (.ite tt ff K)))
         (H; S; η |= (.exec_seq tt K))
| ite_f
  : Step (H; S; η |= (.val .false (.ite tt ff K)))
         (H; S; η |= (.exec_seq ff K))
| while
  : Step (H; S; η |= (.exec (.while e annos body) K))
         (H; S; η |= (.exec (.ite e (body ++ Stmt.List.cons (.while e annos body) .nil) .nil) K))
| return_val₁
  : Step (H; S; η |= (.exec (.return_tau e) K))
         (H; S; η |= (.eval e.val .return))
| return_main
  : Step (H; []; η |= (.val (.num c) .return))
         (H; []; η |= (.res c))
| return_val₂
  : Step (H; (frame :: S); η |= (.val v .return))
         (H; S; frame.environment |= (.val v frame.continuation))
| return_none
  : Step (H; (frame :: S); η |= (.exec (.return_void h) K))
         (H; S; frame.environment |= (.nop frame.continuation))
| assert
  : Step (H; S; η |= (.exec (.assert e) K))
         (H; S; η |= (.eval e.val (.assert K)))
| assert_t
  : Step (H; S; η |= (.val .true (.assert K)))
         (H; S; η |= (.nop K))
| assert_f
  : Step (H; S; η |= (.val .false (.assert K)))
         (H; S; η |= (.exn .abort))
| error₁
  : Step (H; S; η |= (.exec (.error e) K))
         (H; S; η |= (.eval e.val (.error K)))
| error₂
  : Step (H; S; η |= (.val (.str s) (.error K)))
         (H; S; η |= (.exn (.error s)))
/- We skip over annotations because they are not executed. TODO: this might need
    to be changed in order to reason about code, since technically an annotation
    should add a new goal into the Lean state.
 -/
| anno
  : Step (H; S; η |= (.exec (.anno a) K))
         (H; S; η |= (.nop K))
