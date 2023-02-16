import Std

namespace C0deine

structure Symbol where
  name : String
  id : UInt64
deriving DecidableEq, Inhabited

instance : ToString Symbol where toString | s => s.name
instance : Hashable Symbol where hash | s => s.id

universe u
def Symbol.Map (α : Type u) := Std.HashMap Symbol α
