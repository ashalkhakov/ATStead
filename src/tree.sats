// NOTE: include it together with your implementation of [label]!

abstype label = ptr

fun
assl (label): bool = "mac#" // associates to the left
fun
prec (label) : int = "mac#" // precedence
fun
label_is_hole (label): bool = "mac#"
fun
print_label (label): void = "mac#"
overload print with print_label of 100
fun
eq_label_label (label, label): bool = "mac#"
overload = with eq_label_label

(* ****** ****** *)

abstype ident = ptr
fun
eq_ident_ident (ident, ident): bool = "ext#eq_ident_ident"
overload = with eq_ident_ident

(* ****** ****** *)

absvtype tree = ptr
fun
tree_atom (label): tree = "mac#"
fun
tree_fork (label, List_vt tree): tree = "mac#"
fun
tree_is_fork (tree): bool = "mac#"
//
fun
print_tree (!tree): void = "mac#"
overload print with print_tree of 100

fun
eq_tree_tree (!tree, !tree): bool = "mac#"
overload = with eq_tree_tree

fun
hole : () -> tree = "mac#tree_hole"
fun
label : (!tree) -> label = "mac#tree_label"
fun
ident : (!tree) -> ident = "mac#tree_ident"

fun
template : label -> tree = "mac#"

fun
children_foreach : (!tree, (!tree) -<cloref1> void) -> void = "mac#tree_children_foreach"
fun
preorder_foreach : (!tree, label -> void) -> void = "mac#tree_preorder_foreach"
fun
atomic : (!tree) -> bool = "mac#"
fun
tree_delete : (tree) -> void = "mac#"

(* ****** ****** *)

absvtype layer = ptr
absvtype path = ptr
absvtype subtree = ptr

fun
embed : (tree, layer) -> tree = "mac#" // TODO: implement
fun
selected_label : (!subtree) -> label = "mac#tree_label"
fun
subtree_foreach : (!subtree, (!subtree, ident) -<cloref1> void) -> void = "mac#subtree_foreach"
fun
selected_children_foreach : (!subtree, (!subtree) -<cloref1> void) -> void = "mac#tree_children_foreach"
fun
selected_ident : (!subtree) -> ident = "mac#tree_ident"
(*
fun
selected : (!subtree) -> ((tree) -<prf> void | tree) = "mac#" // not checked if we need it?
*)
fun
path : subtree -> path = "mac#subtree_path" // TODO: implement
fun
host : subtree -> tree = "mac#subtree_back_to_top" // get back to root, same as back_to_top
fun
root : tree -> subtree = "mac#subtree_root" // start navigating at the root
(*
(*
host (root t) = t
selected(root t) = t
*)
(*
left (right st) = st, if not (rightmost st)
right (left st) = st, if not (leftmost st)
*)
(*
down (up st) = st, if not (topmost st) && leftmost st
up (down st) = st, if not (bottommost st)
*)
(*
The search stops if a topmost node is reached, but note that

nextSuchThat p st = st

only if st has no proper next successor that satisfies p.
*)

*)

val
left : subtree -> subtree = "mac#subtree_left"
val
leftmost : (!subtree) -> bool = "mac#subtree_leftmost"
val
first_child : subtree -> subtree = "mac#subtree_first_child" // moves to the left until leftmost
val
right : subtree -> subtree = "mac#subtree_right"
val
last_child : subtree -> subtree = "mac#subtree_last_child" // moves to the right until rightmost
val
rightmost : (!subtree) -> bool = "mac#subtree_rightmost"
val
up : subtree -> subtree = "mac#subtree_up"
val
topmost : (!subtree) -> bool = "mac#subtree_topmost"
val
back_to_top : subtree -> subtree = "mac#subtree_back_to_top"
val
down : subtree -> subtree = "mac#subtree_down"
val
bottommost : (!subtree) -> bool = "mac#subtree_bottommost"

(* ****** ****** *)

val
at_hole : (!subtree) -> bool = "mac#subtree_at_hole" // if label="hole", ATS side
val
rightup : subtree -> subtree = "mac#subtree_rightup"
val
next : subtree -> subtree = "mac#subtree_next"
(*
val
preorder : (!tree) -> List(tree) = "mac#"
val
take_one_while : ((!subtree) -> bool, List subtree) -> List subtree = "mac#"
*)
val
next_such_that : ((!subtree) -> bool, subtree) -> subtree = "mac#subtree_next_such_that"
(*
The search stops if a topmost node is reached, but note that

nextSuchThat p st = st

only if st has no proper next successor that satisfies p.
*)

(* ****** ****** *)

// modification
val
replace : (tree, subtree) -> subtree = "mac#subtree_replace"
val
insert : (label, subtree) -> subtree = "mac#subtree_insert"
val
treeinsert : (tree, subtree) -> subtree = "mac#subtree_treeinsert"
val
kill : subtree -> subtree = "mac#"
val
promote : subtree -> subtree = "mac#subtree_promote"
(*
val
situation : (!subtree) -> path = "mac#subtree_situation"
val
graft : (path, subtree) -> subtree = "mac#subtree_graft"
*)

val
enter : (label, subtree) -> subtree = "mac#"
val
entry : (label, subtree) -> subtree = "mac#"
val
reduce : (label, subtree) -> subtree = "mac#"
val
irreducible : (label, !subtree) -> bool = "mac#"
val
producable : (label, label) -> bool = "mac#"
