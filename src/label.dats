
// anything to do with the target language

datatype label_dt =
  Lhole
  | Lif
  | Ladd
  | Lmul
  | Lvar of string
  | Lconst of int
  | Llam
  | Lannot // type annotation
  | Lapp // function application
// deriving (Eq, Ord, Show)

assume label = label_dt

implement
hole () = tree_fork (Lhole (), list_vt_nil ())
implement
label_is_hole (l) =
    case+ l of
    | Lhole () => true
    | _ => false

implement
assl (l) =
    case+ l of
    | Ladd () => true
    | Lmul () => true
    | Lapp () => true
    | _ => false

implement
prec (l) =
    case+ l of
    | Lapp () => 1500
    | Lif () => 1000
    | Ladd () => 100
    | Lmul () => 120
    | Lannot () => 50
    | Llam () => ~1
    | _ => 0

implement
template (l) = let
  #define :: list_vt_cons
  #define nil list_vt_nil
in
    case+ l of
    | Lapp () => tree_fork (Lapp (), hole () :: hole () :: nil())
    | Lif () => tree_fork (Lif (), hole () :: hole () :: hole () :: nil())
    | Llam () => tree_fork (Llam (), hole () :: hole () :: nil())
    | Ladd () => tree_fork (Ladd (), hole () :: hole () :: nil())
    | Lmul () => tree_fork (Lmul (), hole () :: hole () :: nil())
    | Lannot () => tree_fork (Lannot (), hole () :: hole () :: nil())
    | _ => tree_atom (l)
end

implement
print_label (lab) =
    case+ lab of
    | Lhole() => print("hole")
    | Lif () => print("if")
    | Ladd () => print("add")
    | Lmul () => print("mul")
    | Lvar s => print!("var(", s, ")")
    | Lconst i => print!("const(", i, ")")
    | Llam () => print("lam")
    | Lannot() => print("annot")
    | Lapp () => print("app")

implement
eq_label_label (l1, l2) =
    case+ (l1, l2) of
    | (Lhole(), Lhole()) => true
    | (Lif (), Lif ()) => true
    | (Ladd (), Ladd ()) => true
    | (Lmul (), Lmul ()) => true
    | (Lvar s1, Lvar s2) when (s1 = s2) => true
    | (Lconst i1, Lconst i2) when (i1 = i2) => true
    | (Llam (), Llam ()) => true
    | (Lannot (), Lannot ()) => true
    | (Lapp (), Lapp ()) => true
    | (_, _) => false
