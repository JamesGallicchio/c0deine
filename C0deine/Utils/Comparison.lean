/- C0deine - Comparision
   Basic comparison operators that are used in numerous IRs.
   - Thea Brick
 -/
import Lean
namespace C0deine

inductive Comparator
| less
| greater
| equal
| not_equal
| less_equal
| greater_equal
deriving Repr, DecidableEq

namespace Comparator
def isEquality : Comparator → Bool
  | .equal => true
  | .not_equal => true
  | _ => false

def toString : Comparator → String
  | less => "<"
  | greater => ">"
  | equal => "=="
  | not_equal => "!="
  | less_equal => "<="
  | greater_equal => ">="
instance : ToString Comparator where toString := Comparator.toString

instance : Lean.ToExpr Comparator :=
{
  toExpr := fun c =>
    match c with
    | Comparator.less    => Lean.mkConst ``Comparator.less
    | Comparator.equal   => Lean.mkConst ``Comparator.equal
    | Comparator.greater => Lean.mkConst ``Comparator.greater
    | Comparator.not_equal => Lean.mkConst ``Comparator.not_equal
    | Comparator.less_equal => Lean.mkConst ``Comparator.less_equal
    | Comparator.greater_equal => Lean.mkConst ``Comparator.greater_equal
  toTypeExpr := Lean.mkConst ``Comparator
}
