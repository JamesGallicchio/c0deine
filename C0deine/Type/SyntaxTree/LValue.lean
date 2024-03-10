/- C0deine - TST.LValue
   LValues or expressions that appear on the left of assignments. These must be
   well typed by definition.
   - Thea Brick
 -/
import C0deine.Type.SyntaxTree.Expr

namespace C0deine.Tst

open Typ

open Typ.Notation in
inductive LValue (Δ : GCtx) (Γ : FCtx) : Typ → Type
| var   : (x : Symbol)
        → (Γ.syms x = .some (.var τ))
        → LValue Δ Γ τ
| dot   : {τ₁ : {τ : Typ // τ = (struct s)}}
        → LValue Δ Γ τ₁
        → (field : Symbol)
        → Δ.struct s = .some ⟨fields, true⟩
        → fields field = .some τ
        → LValue Δ Γ τ
| deref : {τ₁ : {τ' : Typ // τ' = (τ*)}}
        → LValue Δ Γ τ₁
        → LValue Δ Γ τ
| index : {τ₁ : {τ' : Typ // τ' = (τ[])}}
        → {τ₂ : {τ : Typ // τ = (int)}}
        → LValue Δ Γ τ₁
        → Expr.NoContract Δ Γ τ₂
        → LValue Δ Γ τ

namespace LValue

open Typ.Notation

@[inline] def is_var : LValue Δ Γ τ → Bool
  | .var _ _ => true | _ => false
@[inline] def get_name
    (lval : LValue Δ Γ τ) (h₁ : lval.is_var) : Symbol :=
  match h₂ : lval with
  | .var name _   => name
  | .dot _ _ _ _
  | .deref _
  | .index _ _    => by simp [is_var] at h₁

@[inline] def typeWith {p : Typ → Prop} (e : LValue Δ Γ τ) (h : p τ)
    : LValue Δ Γ (⟨τ, h⟩ : {τ : Typ // p τ}) := e
@[inline] def typeWithEq {τ₂ : Typ} (e : LValue Δ Γ τ) (eq : τ = τ₂)
    : LValue Δ Γ (⟨τ, eq⟩ : {τ : Typ // τ = τ₂}) :=
  e.typeWith (p := fun t => t = τ₂) eq

@[inline] def intType (e : LValue Δ Γ τ) (eq : τ = (int))
    : LValue Δ Γ (⟨τ, eq⟩ : {τ : Typ // τ = (int)}) := e.typeWithEq eq
@[inline] def ptrType (e : LValue Δ Γ τ) (τ' : Typ) (eq : τ = (τ'*))
    : LValue Δ Γ (⟨τ, eq⟩ : {τ : Typ // τ = (τ'*)}) := e.typeWithEq eq
@[inline] def arrType (e : LValue Δ Γ τ) (τ' : Typ) (eq : τ = (τ'[]))
    : LValue Δ Γ (⟨τ, eq⟩ : {τ : Typ // τ = (τ'[])}) := e.typeWithEq eq
@[inline] def structType (e : LValue Δ Γ τ) (s : Symbol) (eq : τ = (struct s))
    : LValue Δ Γ (⟨τ, eq⟩ : {τ : Typ // τ = (struct s)}) := e.typeWithEq eq

structure Predicate (Δ : GCtx) (Γ : FCtx) (α : Type) where
  lval : (τ : Typ) → α → LValue Δ Γ τ → Option α
  expr : (τ : Typ) → α → Expr Δ Γ τ → Option α

/- Assert that some predicate P applies to every sub-lvalue -/
inductive Fold : {Δ : GCtx} → {Γ : FCtx}
  → (P : LValue.Predicate Δ Γ α) → α → LValue Δ Γ τ → α → Prop
| var
  : {a₁ a₂ : α}
  → {P : LValue.Predicate Δ Γ α}
  → {h : Γ.syms x = .some (.var τ)}
  → P.lval _ a₁ (.var x h) = some a₂
  → Fold P a₁ ((.var x h) : LValue Δ Γ _) a₂
| dot
  : Fold P a₁ l a₂
  → P.lval _ a₂ (.dot l f h₁ h₂) = some a₃
  → Fold P a₁ (.dot l f h₁ h₂) a₃
| deref
  : Fold P a₁ l a₂
  → P.lval _ a₂ (.deref l) = some a₃
  → Fold P a₁ (.deref l) a₃
| index
  : Fold P a₁ l a₂
  → Expr.Fold P.expr a₂ e.val a₃
  → P.lval _ a₃ (.index l e) = some a₄
  → Fold P a₁ (.index l e) a₄

end LValue

def LValue.toString : LValue Δ Γ τ → String
  | .var name _ => s!"({name} : {τ})"
  | .dot e field _ _ =>
    s!"({LValue.toString e}.{field} : {τ})"
  | .deref e => s!"(*{LValue.toString e} : {τ})"
  | .index e i => s!"({LValue.toString e}[{i}] : {τ})"

instance : ToString (LValue Δ Γ τ) where toString := LValue.toString
instance : ToString (List (Typed Symbol)) where
  toString tss := tss.map Typed.toString |> String.intercalate ", "
