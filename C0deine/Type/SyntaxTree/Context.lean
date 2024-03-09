/- C0deine - TST.Context
   Utilies for implementing the TST, specifically different contexts.
   - Thea Brick
 -/
import Numbers
import C0deine.AuxDefs
import C0deine.Type.Typ
import C0deine.Context.Symbol
import C0deine.Utils.Comparison

namespace C0deine.Tst

open Typ

structure FuncSig where
  arity  : Nat
  argTys : Fin arity → Typ
  retTy  : Typ    -- use .any if void

structure Status.Func where
  type    : FuncSig
  defined : Bool

structure Status.Struct where
  fields  : Symbol → Option Typ
  defined : Bool

inductive Status.Symbol
| var   (v : Typ)
| func  (f : Status.Func)
| alias (t : Typ)

-- use Status.Symbol to prevent collisions with funcs/tydefs
abbrev FCtx := Symbol → Option Status.Symbol

@[inline] def FCtx.update (Γ : FCtx) (x : Symbol) (s : Status.Symbol) : FCtx :=
  Function.update Γ x (some s)
@[inline] def FCtx.updateVar (Γ : FCtx) (x : Symbol) (τ : Typ) : FCtx :=
  Γ.update x (.var τ)
@[inline] def FCtx.updateFunc
    (Γ : FCtx) (x : Symbol) (s : Status.Func) : FCtx :=
  Γ.update x (.func s)
@[inline] def FCtx.ofParams (params : List (Typed Symbol)) : FCtx :=
  (params.map (fun p => (p.data, .var p.type))).toMap
@[inline] def FCtx.addFunc
    (Γ : FCtx) (f : Symbol) (retTy : Typ) (params : List (Typed Symbol))
    : FCtx :=
  let params_Γ := FCtx.ofParams params
  let args := fun i => params.get i |>.type
  let status := ⟨⟨params.length, args, retTy⟩, true⟩
  fun x => -- re-add params bc they shadow the function definition
    match params_Γ x with
    | some status => some status
    | none => if x = f then some (.func status) else Γ x

structure GCtx where
  symbols : Symbol → Option Status.Symbol := fun _ => none
  struct  : Symbol → Option Status.Struct := fun _ => none
deriving Inhabited

@[inline] def FCtx.init
    (Δ : GCtx) (params : List (Typed Symbol)) : FCtx :=
  let params_Γ := FCtx.ofParams params
  fun x =>
    match params_Γ x with
    | some status => some status
    | none => Δ.symbols x