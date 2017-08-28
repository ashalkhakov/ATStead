(*
** Prototype.
**
** Translated directly from the paper.
** Some testing code thrown in.
**
** You can paste it into Try ATS.
*)

(* ****** ****** *)
//
#define
LIBATSCC2JS_targetloc
"$PATSHOME/contrib/libatscc2js/ATS2-0.3.2"
//
#include
"{$LIBATSCC2JS}/staloadall.hats"
//
(* ****** ****** *)

staload
"{$LIBATSCC2JS}/SATS/print.sats"
staload
"{$LIBATSCC2JS}/SATS/ML/list0.sats"
staload
"{$LIBATSCC2JS}/SATS/filebas.sats"
staload
"{$LIBATSCC2JS}/SATS/list.sats"
staload
"{$LIBATSCC2JS}/SATS/JSarray.sats"

(* ****** ****** *)

#define ATS_MAINATSFLAG 1
#define ATS_DYNLOADNAME "my_dynload"

(* ****** ****** *)

extern
fun
escape_html (string): string = "mac#"
extern
fun
JSarray_clear {a:t@ype} (JSarray(a)): void = "mac#"

abstype html_env = ptr
extern
fun
html_env_init () : html_env = "mac#"
extern
fun
html_tag_open (html_env, tagname : string, attributes: List '(string,string)): void = "mac#"
extern
fun
html_text (html_env, string): void = "mac#"
extern
fun
html_tag (html_env, tagname: string): void = "mac#"
extern
fun
html_tag_close (html_env): void = "mac#"
extern
fun
html_env_html (html_env): string = "mac#"

assume html_env = '(JSarray string, JSarray string)
implement
html_env_init () = '(JSarray_nil(), JSarray_nil ())
implement
html_tag_open (h, tagname, attribs) = let
    val x = "<" + tagname + (if list_is_nil attribs then "" else " ")
    val _ = JSarray_push (h.1, tagname)
    fun
    aux (ls: List '(string, string)): void =
        case+ ls of
        | list_nil () => let
                val _ = JSarray_push (h.0, ">")
            in
            end
        | list_cons ('(attr, value), ls) => let
                val _ = JSarray_push (h.0, escape_html attr + "=\"" + escape_html value + "\"")
            in
                aux (ls)
            end
    val _ = JSarray_push (h.0, x)
    val () = aux (attribs)
in
end
implement
html_text (h, text) = let
    val text = escape_html text
    val _ = JSarray_push (h.0, text)
in
end
implement
html_tag_close (h) =
let
    val n = JSarray_length (h.1)
in
    case+ n of
    | 0 => alert "woohoo"
    | _  => let
            val tn = JSarray_pop (h.1)
            val _ = JSarray_push (h.0, "</" + tn + ">")
        in
        end
end
implement
html_env_html (h) = JSarray_join (h.0)

val the_html_env = html_env_init ()
extern
fun
html_env_reset (): void = "mac#"
implement
html_env_reset () = {
    val () = JSarray_clear (the_html_env.0)
    val () = JSarray_clear (the_html_env.1)
}

local

macrodef
rec
auxlist
  (xs, y) =
(
//
if iscons! (xs) then
  `(,(car! xs); ,(auxlist (cdr! xs, y)))
else y // end of [if]
//
) // end of [auxlist]

in (* in of [local] *)

macrodef
TAG (tag, a, r) = `({
    val () = html_tag_open (the_html_env, ,(tag), ,(a))
    val () = ,(if islist! (r) then auxlist (r, `())
                else `(,(r)))
    val () = html_tag_close (the_html_env)
})
macdef
P (a, r) = ,(TAG (`("p"), a, r))
macdef
STRONG (a, r) = ,(TAG (`("strong"), a, (r)))
macdef
DIV (a, r) = ,(TAG (`("div"), a, r))
macdef
TEXT (ls) = {
    val () = html_text (the_html_env, ,(ls))
}

end // end of [local]

extern
fun
test_html(): void = "mac#"
implement
test_html() = let

val _ = P (list_cons('("class", "selected"), list_nil), TEXT ("hi"), STRONG (list_nil, TEXT ("there")), TEXT ("!"))
(*
val () = html_tag_open ("p", list_nil)
val () = html_text (env, "hello ")
    val () = html_tag_open (env, "strong", list_nil)
        val () = html_text (env, "person")
    val () = html_tag_close (env)
val () = html_text (env, "!")
val () = html_tag_close (env)
*)
val () = alert (html_env_html (the_html_env))
in
end

(*
// simple HTML output, how?
// print it!
// or, minimal DOM entry
absvtype dom (bool(*text node*), l:addr) = ptr
vtypedef dom0 (b:bool) = [l:addr] dom (b, l)
vtypedef dom1 (b:bool) = [l:agz] dom (b, l)
vtypedef domelt0 = [l:addr] dom (false, l)
vtypedef domtext0 = [l:addr] dom (true, l)
vtypedef domelt1 = [l:agz] dom (false, l)
vtypedef domtext = [l:agz] dom (true, l)
absvtype domseq (l:addr) = ptr
vtypedef domseq0 = [l:addr] domseq (l)

extern
fun
appendChild (parent : !domelt1, newChild : dom1): void
(* moves the tree newChild
to the end of parent’s child list. Requires that parent
exists and is not a text node, and that newChild exists
and is not an ancestor of parent, or it faults.
*)
extern
fun
removeChild (parent : !domelt1, oldChildIndex : int) : dom0
(* removes the tree oldChild
from the tree parent’s child forest and re-inserts it at
the root of the grove. Requires that parent exists and
oldChild is a child of parent, or it faults.
*)
extern
fun
getNodeName (node : !domelt1) : string
(* assigns to the variable name
the nodeName value of node if it exists, or it faults. If
node is a text node, then name = #text.
*)
extern
fun
getParentNode (node : !domelt1) : Option string
(* assigns to the variable id the
identifier of the parent of node, if it exists, and null
otherwise. Requires that node exists or it faults.
*)
extern
fun
getChildNodes {l:agz} (node : dom (false, l)) : [l1:addr] (domseq (l1) -<lin,prf> dom (false, l) | domseq0 (l1))
(*
fid := getChildNodes (node : dom) : doms assigns to the variable fid
the identifier of the child forest of the element node,
which must exist or it faults.
*)
extern
fun
createNode (Name : string) : domelt1
(*creates a new element node at
the root of the grove, with fresh id and fid and a
name equal to Name, and records its identifier in the
variable node. It faults if JNameKs 6∈ EltNames.
*)
extern
fun
item {l:agz} (list : domseq0 (l), i: int) : dom // takeout?
(* sets the variable node to the Int+
1th node in the list pointed to by list, setting it to
null if Int evaluates to an invalid index. It faults if
list does not exist.
*)
extern
fun
substringData (node : !domtext1, Offset : int, Count : int) : string
(* assigns to the
variable str the substring of the string of the text node
node starting at character Offset with length Count.
If Offset + Count exceeds the string length, then all
the characters to the string end are returned. Requires
that node exists, Offset and Count be non-negative,
and Offset be at most the string length, or it faults.
*)
extern
fun
appendData (node : !domtext1, Arg : string) : void
(* appends the string Arg to the end
of the string contained in node. Requires that node
exists and be a text node, or it faults.
*)
extern
fun
deleteData (node : !domtext1, Offset : int, Count : int) : void
(* deletes the substring of
the string of node starting at the character Offset
with length Count. If Offset + Count exceeds the
string length, then all the characters to the string end
are deleted. Requires that node exists, Offset and
Count be non-negative, and Offset be at most the
string length, or it faults.
*)
extern
fun
createTextNode (Str : text) : domtext1
(* creates a new text node at
the grove level, with fresh id and the string contained
within the text node set to Str, and records the new
node’s identifier in the variable node.
*)
*)

(* ****** ****** *)

abstype label = ptr
extern
fun
assl (label): bool = "mac#" // associates to the left
extern
fun
prec (label) : int = "mac#" // precedence
extern
fun
is_hole (label): bool = "mac#"
extern
fun
print_label : (label) -> void = "mac#"
overload print with print_label of 100
extern
fun
eq_label_label : (label, label) -> bool = "mac#"
overload = with eq_label_label
extern
fun
class_label : (label) -> string = "mac#"

abstype tree = ptr
extern
fun
tree_atom (label): tree = "mac#"
extern
fun
tree_fork (label, List tree): tree = "mac#"
extern
fun
tree_is_fork (tree): bool = "mac#"

//
extern
fun
print_tree (tree): void = "mac#"
overload print with print_tree of 100

extern
fun
eq_tree_tree (tree, tree): bool = "mac#"
overload = with eq_tree_tree

extern
fun
template (label) : tree = "mac#"

extern
val
hole : tree
extern
fun
label : tree -> label = "mac#"
extern
fun
children : tree -> List tree = "mac#"
extern
fun
atomic : tree -> bool = "mac#"

(* ****** ****** *)

extern
fun
myprint (tree): void = "mac#"

implement
print_val<tree> (t) = myprint (t)
implement
myprint (t) = {
    val label = label t
    val cs = children t
    val () = print("fork(")
    val () = print(label)
    val () = print(",")
    implement
    print_val<tree> (t) = myprint (t)
    val () = print("[")
    val () = print(cs)
    val () = print("]")
    val () = print(")")
}

implement
print_tree (t) = myprint (t)

local

datatype tree0 =
    | fork of (label, JSarray tree0)
// deriving (Eq, Ord, Show)

assume tree = tree0

in

implement
label (fork (l, _)) = l

implement
children (fork (_, ts)) = let
    val n = JSarray_length ts

    fun
    aux (i : int, res : List tree) : List tree =
        if i >= 0 then let
            val t = JSarray_get_at(ts, i)
            prval () = lemma_list_param res
            val res = list_cons (t, res)
        in
            aux (i-1, res)
        end else res
    val res = aux (n-1, list_nil)
in
    res
end
implement
atomic (fork (_, cs)) = (JSarray_length cs = 0)

implement
tree_atom (l) = fork (l, JSarray_nil())
implement
tree_fork (l, cs) = fork (l, JSarray_make_list cs)

implement
tree_is_fork (t) = ~atomic t

implement
eq_tree_tree (t1, t2) = let
    fun
    aux (i: int, n: int, t1: JSarray (tree), t2: JSarray (tree)): bool =
        if i < n then let
            val a = JSarray_get_at (t1, i)
            val b = JSarray_get_at (t2, i)
        in
            if eq_tree_tree (a, b) then aux (i+1, n, t1, t2)
            else false
        end else true
    // end of [aux]
in
    case+ (t1, t2) of
    | (fork (l1, c1), fork (l2, c2)) when (l1 = l2) => let
            val n1 = JSarray_length c1
            val n2 = JSarray_length c2
        in
            if n1 = n2 then aux (0, n1, c1, c2)
            else false
        end
    | (_, _) => false
end

end // end of [local]

typedef layer = '(label(*label of focussed subtree*), List tree(*children to the left*), List tree(*children to the right*))
typedef path = List layer
typedef subtree = '(path, tree)

extern
fun
embed : (tree, layer) -> tree = "mac#"
extern
fun
selected : subtree -> tree = "mac#"
extern
fun
path : subtree -> path = "mac#"
extern
fun
host : subtree -> tree = "mac#"
extern
fun
root : tree -> subtree = "mac#"

implement
embed (t, '(l,left,right)) =
    let
        val left = list_reverse left
        prval () = lemma_list_param right
        val res = list_append (left, list_cons (t, right))
    in
        tree_fork (l, res)
    end

implement
selected '(_, t) = t
implement
path '(p, _) = p
implement
host '(p,t) = list_foldleft (p, t, lam (x,y) =<cloref1> embed (x, y))
implement
root (t) = let
in
    '(list_nil(), t)
end

(*
host (root t) = t
selected(root t) = t
*)

(* ****** ****** *)
(* navigation *)

extern
val
left : subtree -> subtree = "mac#"
extern
val
leftmost : subtree -> bool = "mac#"
extern
val
first_child : subtree -> subtree = "mac#" // moves to the left until leftmost
extern
val
right : subtree -> subtree = "mac#"
extern
val
last_child : subtree -> subtree = "mac#" // moves to the right until rightmost
extern
val
rightmost : subtree -> bool = "mac#"
extern
val
up : subtree -> subtree = "mac#"
extern
val
topmost : subtree -> bool = "mac#"
extern
val
back_to_top : subtree -> subtree = "mac#"
extern
val
down : subtree -> subtree = "mac#"
extern
val
bottommost : subtree -> bool = "mac#"
extern
val
at_hole : subtree -> bool = "mac#"
extern
val
next : subtree -> subtree = "mac#"
extern
val
rightup : subtree -> subtree = "mac#"
extern
val
preorder : tree -> List(tree) = "mac#"
extern
val
take_one_while : (subtree -> bool, List subtree) -> List subtree = "mac#"
extern
val
next_such_that : (subtree -> bool, subtree) -> subtree = "mac#"

implement
left st =
let
    val '(p, sel) = st
in
    case- p of
    | list_cons('(lab, list_cons(last,l), r),ls) => let
            prval () = lemma_list_param r
        in
            '(list_cons('(lab, l, list_cons(sel,r)),ls), last)
        end
    | list_cons('(_, list_nil(), _),_) => st
    | list_nil () => st
end

implement
leftmost st =
let
    val '(p, sel) = st
in
    case+ p of
    | list_cons ('(_, list_nil(), _), _) => true
    | list_nil () => true
    | _ => false
end

implement
first_child st =
    if leftmost st then st
    else let
        val st = left st
    in
        first_child st
    end

implement
right st =
let
    val '(p, sel) = st
in
    case- p of
    | list_cons ( '(lab, l, list_cons(first,r)), ls) => let
            prval () = lemma_list_param l
        in
            '(list_cons ('(lab, list_cons (sel,l), r), ls), first)
        end
    | list_cons ( '(_, _, list_nil()), _) => st
    | list_nil () => st
end

implement
last_child st =
    if rightmost st then st
    else let
        val st = right st
    in
        last_child st
    end

implement
rightmost st =
let
    val '(p, sel) = st
in
    case+ p of
    | list_cons ('(_, _, list_nil()), _) => true
    | list_nil () => true
    | _ => false
end

(*
left (right st) = st, if not (rightmost st)
right (left st) = st, if not (leftmost st)
*)

implement
up st =
let
    val+ '(p, sel) = st
in
    case+ p of
    | list_cons (layer, above) => '(above, embed (sel, layer))
    | list_nil () => st
end

implement
topmost st =
let
    val+ '(p, sel) = st
in
    list_is_nil p
end

implement
down st =
let
    val+ '(p, tr) = st
    val lab = label tr
    val c = children tr
in
    case+ c of
    | list_cons (t, ts) => let
            prval () = lemma_list_param p
        in
            '(list_cons ('(lab, list_nil(), ts), p), t)
        end
    | list_nil () => st
end

implement
bottommost st =
let
    val+ '(p, tr) = st
in
    atomic tr
end

(*
down (up st) = st, if not (topmost st) && leftmost st
up (down st) = st, if not (bottommost st)
*)

implement
back_to_top st =
    if topmost st then st
    else let
        val st = up st
    in
        back_to_top st
    end

implement
at_hole st =
let
    val+ '(ls, tr) = st
    val lab = label tr
    val c = children tr
in
    is_hole lab && list_is_nil c
end

implement
next st =
    if bottommost st then
        rightup st
    else
        down st

implement
rightup st =
    if topmost st || ~rightmost st then right st
    else let
        val st = up st
    in
        rightup st
    end

(*
implement
preorder = map selected . takeOneWhile (not . topmost) . iterate next . root

implement
take_one_while (p, xs) = takeWhile p xs
*)

implement
next_such_that (p, st) =
let
    fun
    aux (st : subtree): subtree =
        if topmost st || p st then st
        else let
            val st = next st
        in
            aux st
        end
    val st0 = next st
    val st' = aux st0
in
    if p st' then st' else st
end

(*
The search stops if a topmost node is reached, but note that

nextSuchThat p st = st

only if st has no proper next successor that satisfies p.
*)

(* ****** ****** *)
(* modification *)

extern
val
replace : (tree, subtree) -> subtree = "mac#"
extern
val
insert : (label, subtree) -> subtree = "mac#"
extern
val
treeinsert : (tree, subtree) -> subtree = "mac#"
extern
val
kill : subtree -> subtree = "mac#"
extern
val
promote : subtree -> subtree = "mac#"
extern
val
situation : subtree -> path = "mac#"
extern
val
graft : (path, subtree) -> subtree = "mac#"

implement
replace (s, '(p, t)) = '(p, s)
(*
replace (selected st) (replace s st) = st
*)

implement
insert (lab, st) =
let
    val r = template lab
    val st' = treeinsert (r, st)
in
  st'
end

implement
treeinsert (t, st) =
    if at_hole st then
        replace (t, st)
    else let
        val sel = selected st
        val st = replace (t, st)
        val st = down st
        val st = replace (sel, st)
    in
        st
    end

(*
selection(treeinsert t st) = selection st, if not (atHole st)
*)

implement
kill st = replace (hole, st)

implement
promote st =
let
    val sel = selected st
    val st = up st
in
    replace (sel, st)
end

(*
kill (treeinsert t st) = st, if atHole st
promote (treeinsert t st) = st, if not (atHole st) && not(atomic t)
id (treeinsert t st) = st, if not (atHole st) && atomic t
replace (selected st) (kill st) = st
*)

implement
situation st =
let
    val+ '(p, sel) = st
in
    case+ p of
    | list_cons (c, cs) => list_sing(c)
    | list_nil () => p
end

implement
graft (cs, st) =
let
    val+ '(p, sel) = st
in
    '(list_append (cs, p), sel)
end

(*
graft (situation st) (promote st) = st
*)

(* ****** ****** *)
(* entry *)

extern
val
enter : (label, subtree) -> subtree = "mac#"
extern
val
entry : (label, subtree) -> subtree = "mac#"
extern
val
reduce : (label, subtree) -> subtree = "mac#"
extern
val
irreducible : (label, subtree) -> bool = "mac#"
extern
val
producable : (label, label) -> bool = "mac#"

implement
enter (l, st) =
let
    val st = insert (l, st)
in
    next_such_that (at_hole, st)
end

implement
entry (l, st) =
let
    val st = reduce (l, st)
in
    enter (l, st)
end

implement
reduce (l, st) =
    if irreducible (l, st) then st
    else let
        val st = up st
    in
        reduce (l, st)
    end

implement
irreducible (l, st) =
    at_hole st || topmost st || ~rightmost st ||
        ~producable (l, label (selected (up st)))

implement
producable (op2, op1) = (op1 = op2 && assl op1) || prec op1 > prec op2

(* ****** ****** *)

typedef subtree' = '(List (path), tree)

extern
val
unflatten : subtree -> subtree' = "mac#"
extern
val
flatten : subtree' -> subtree = "mac#"
extern
fun{}
lift$fopr : subtree -> subtree
extern
fun{}
lift : subtree' -> subtree'

implement
unflatten '(ps, t) = '(list_sing(ps), t)

implement
flatten '(ps,t) = '(list_concat ps, t)

implement{}
lift st =
let
    val '(ps, t) = st
    val- list_cons (p, ps') = ps
    val '(q, s) = lift$fopr<> '(p, t)
in
    '(list_cons (q, ps'), s)
end

extern
val
enter' : (label, subtree') -> subtree'
extern
val
entry' : (label, subtree') -> subtree'
extern
val
left' : subtree' -> subtree'
extern
val
first_child' : subtree' -> subtree'
extern
val
right' : subtree' -> subtree'
extern
val
last_child' : subtree' -> subtree'
extern
val
up' : subtree' -> subtree'
extern
val
down' : subtree' -> subtree'
extern
val
back_to_top' : subtree' -> subtree'
extern
val
insert' : (label, subtree') -> subtree'
extern
val
replace' : (tree, subtree') -> subtree'
extern
val
open : subtree' -> subtree'
extern
val
close : subtree' -> subtree'

implement
entry' (l, st) = lift<> (st) where {
    implement
    lift$fopr<> st = entry (l, st)
}
implement
left' st = let
    implement
    lift$fopr<> st = left st
in
    lift<> st
end
implement
first_child' st = let
    implement
    lift$fopr<> st = first_child st
in
    lift<> st
end
implement
right' st = let
    implement
    lift$fopr<> st = right st
in
    lift<> st
end
implement
last_child' st = let
    implement
    lift$fopr<> st = last_child st
in
    lift<> st
end
implement
up' st = let
    implement
    lift$fopr<> st = up st
in
    lift<> st
end
implement
down' st = let
    implement
    lift$fopr<> st = down st
in
    lift<> st
end
implement
back_to_top' st = let
    implement
    lift$fopr<> st = back_to_top st
in
    lift<> st
end
implement
insert' (l, st) = let
    implement
    lift$fopr<> st = insert (l, st)
in
    lift<> st
end
implement
replace' (l, st) = let
    implement
    lift$fopr<> st = replace (l, st)
in
    lift<> st
end

implement
open st =
let
    val+ '(p, t) = st
    prval () = lemma_list_param p
in
    '(list_cons(list_nil(), p), t)
end
implement
close st =
let
    val+ '(p, t) = st
    val- list_cons (p, ps) = p
    val s = host '(p, t)
in
    right' '(ps, s)
end

(* ****** ****** *)
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
hole = tree_fork (Lhole (), list_nil ())
implement
is_hole (l) =
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

#define :: list_cons

implement
template (l) = let
in
    case+ l of
    | Lapp () => tree_fork (Lapp (), hole :: hole :: list_nil())
    | Lif () => tree_fork (Lif (), hole :: hole :: hole :: list_nil())
    | Llam () => tree_fork (Llam (), hole :: hole :: list_nil())
    | Ladd () => tree_fork (Ladd (), hole :: hole :: list_nil())
    | Lmul () => tree_fork (Lmul (), hole :: hole :: list_nil())
    | Lannot () => tree_fork (Lannot (), hole :: hole :: list_nil())
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

implement
class_label (lab) =
    case+ lab of
    | Lhole() => "hole"
    | Lif () => "if"
    | Ladd () => "add"
    | Lmul () => "mul"
    | Lvar s => "var"
    | Lconst i => "const"
    | Llam () => "lam"
    | Lannot() => "annot"
    | Lapp () => "app"

(* ****** ****** *)
// simple driver for testing

datatype navtype =
    | NTleft
    | NTright
    | NTup
    | NTdown
    | NTfirst
    | NTlast
    | NTtop
// deriving (Eq, Ord, Show)
datatype command =
    | Centry of tree // begin with this tree
    | Clparen // open paren
    | Crparen // close paren
    | Cmove of navtype // navigate the tree
    | Creplace of tree // replace focussed subtree with the new tree
    | Cinsert of label // insert a new label
    | Center of label // enter a new label, taking precedence into account
// deriving (Eq, Ord, Show)
datatype modelstate =
    | MSnothing of ()
    | MSparenthesized of subtree'
// deriving (Eq, Ord, Show)
typedef model =
    '(modelstate, string)

extern
fun
present (command, model) : model = "mac#"

implement
present (cmd, '(tree, errorMsg)) = let

in
    case+ cmd of
    | Centry t => let
            val r = root t
            val r' = unflatten r
            val res = MSparenthesized r'
        in
            '(res,  "")
        end
    | Cmove nt => (
        case+ tree of
        | MSnothing () => '(tree, "Cannot move without a tree")
        | MSparenthesized x =>
            let
                val x =
                    case+ nt of
                    | NTleft () => left' x
                    | NTright () => right' x
                    | NTup () => up' x
                    | NTdown () => down' x
                    | NTfirst () => first_child' x
                    | NTlast () => last_child' x
                    | NTtop () => back_to_top' x
            in
                '(MSparenthesized x, "")
            end
        )
    | Creplace t => (
        case+ tree of
        | MSnothing () => '(tree, "Cannot replace without a tree")
        | MSparenthesized x => '(MSparenthesized (replace' (t, x)), "")
        )
    | Cinsert lab => (
        case+ tree of
        | MSnothing () => '(tree, "Cannot insert without a tree")
        | MSparenthesized x => '(MSparenthesized (insert' (lab, x)), "")
        )
    | Center lab => (
        case+ tree of
        | MSnothing () => '(tree, "Cannot enter without a tree")
        | MSparenthesized x => '(MSparenthesized (entry' (lab, x)), "")
        )
    | Clparen () => (
        case tree of
        | MSnothing () => '(tree, "Cannot insert left paren without a tree")
        | MSparenthesized t => '(MSparenthesized (open t), "")
        )
    | Crparen () => (
        case+ tree of
        | MSnothing () => '(tree, "Cannot insert right paren without a tree")
        | MSparenthesized t => '(MSparenthesized (close t), "")
        )
    //| _ => '(tree, "Unable to perform command: " (* ++ show c *))
end

extern
fun
script_present (model, List command): model
implement
script_present (mdl, cmds) =
    case+ cmds of
    | list_nil () => mdl
    | list_cons (c, cs) => let
            val mdl' = present (c, mdl)
        in
            script_present (mdl', cs)
        end // end of [list_cons]

extern
fun
render_tree (tree): void
implement
render_tree (t) = {
    fun
    aux (cs : List tree): void =
        case+ cs of
        | list_nil () => ()
        | list_cons (x, cs) => {
                val () = render_tree (x)
                val () = aux (cs)
            }
    val lab = label t
    val cs = children t
    val () = DIV ( '("class", class_label lab) :: list_nil, {
        val () = aux cs
    })
}

extern
fun
render (model): void
implement
render ('(tree, errmsg)) = let
in
    case+ tree of
    | MSnothing () => let
        val _ = DIV (list_nil, P(list_nil, TEXT "no data!"))
    in
    end
    | MSparenthesized '(p, t) => let
        // '(list0 (path), tree)
        // 1. output the path
        // 2. output the tree
        (*
typedef layer = '(label(*label of focussed subtree*), list0 tree(*children to the left*), list0 tree(*children to the right*))
typedef path = list0 layer
typedef subtree = '(path, tree)
typedef subtree' = '(list0 (path), tree) // we are here
        *)
        val () = DIV( '("class", "selected syntax-tree") :: list_nil, render_tree t)
        val _ = P (list_nil, TEXT "NO DATA!")
    in
    end
end

extern
fun
script_present_result (model, List command): tree
implement
script_present_result (mdl, xs) =
let
    val '(d, e) = script_present (mdl, xs)
    val- MSparenthesized x = d
in
    host (flatten x)
end

extern
val
expect : (tree, tree) -> void
implement
expect (x, y) =
    if x = y then println! ("OK: ", x)
    else {
        val () = println! ("Error! Mismatched terms")
        val () = println! ("Expected: ", x)
        val () = println! ("Actual: ", y)
        val () = assert_errmsg (false, "should have been equal")
    }
(* ****** ****** *)
//
extern
fun
f_var : string -> tree
extern
fun
f_const : int -> tree
extern
fun
f_add : (tree, tree) -> tree
extern
fun
f_mul : (tree, tree) -> tree
extern
fun
f_hole : () -> tree
extern
fun
f_if : (tree, tree, tree) -> tree
extern
fun
f_lam : (tree, tree) -> tree
extern
fun
f_app : (tree, tree) -> tree
extern
fun
f_annot : (tree, tree) -> tree
//
implement
f_var (s) = template (Lvar s)
implement
f_const (i) = template (Lconst i)
implement
f_add (l, r) = tree_fork (Ladd, l :: r :: list_nil)
implement
f_mul (l, r) = tree_fork (Lmul, l :: r :: list_nil)
implement
f_hole () = template (Lhole)
implement
f_if (a, b, c) = tree_fork (Lif, a :: b :: c :: list_nil)
implement
f_lam (a, b) = tree_fork (Llam, a :: b :: list_nil)
implement
f_app (a, b) = tree_fork (Lapp, a :: b :: list_nil)
implement
f_annot (a, b) = tree_fork (Lannot, a :: b :: list_nil)
//
extern
fun
main(): void = "mac#"
implement
main() = {
    val () = println!("test 0")
    val init = '(MSnothing, "")
    val () = println!("printing hole: ", Lhole ())
    val () = println!("printing variable x: ", Lvar "x")
    val () = println!("printing variable add: ", Ladd ())
    val t = template Ladd
    val () = myprint(t) // fails hard... why???

    val e1 = script_present_result (init, Centry t :: list_nil)
    val () = expect (tree_fork (Ladd, tree_fork (Lhole, list_nil) :: tree_fork (Lhole, list_nil) :: list_nil), e1)

    val () = println!("test A")
    val e1 = script_present_result (init,
        Centry (template Ladd) :: Cmove NTdown :: Cinsert (Lconst 1) :: Cmove NTright :: Cinsert (Lconst 2) :: Cmove NTup :: list_nil ())
    val () = expect (f_add (f_const 1, f_const 2), e1)
   
  val () = println!("test B")
  val e2 = script_present_result
        (init,
        Centry hole ::
        Cinsert (Lvar "A") ::
        Cinsert Ladd ::
        Cmove NTright ::
        Cinsert (Lvar "B") ::
        Cinsert Ladd ::
        Cmove NTright ::
        Cinsert (Lvar "C") :: list_nil ()) // result should be A + (B + C)
  val () = expect (f_add (f_var "A", f_add (f_var "B", f_var "C")), e2)

  val() = println!("test C")
  // a*b+c*d
  val e3 = script_present_result (
    init,
    Centry hole :: Center (Lvar "A") :: Center Lmul :: Center (Lvar "B")
    :: Center Ladd :: Center (Lvar "C") :: Center Lmul :: Center (Lvar "D") :: list_nil)
  val () = expect
    (f_add (
      f_mul (f_var "A", f_var "B"),
      f_mul (f_var "C", f_var "D")),
    e3)

  val () = println!("test D")
  // (a+b)*(c+d)
  val e4 = script_present_result (
    init,
    Centry hole ::
      Clparen :: Center (Lvar "A") :: Center Ladd :: Center (Lvar "B") :: Crparen ::
      Center Lmul ::
      Clparen :: Center (Lvar "C") :: Center Ladd :: Center (Lvar "D") :: Crparen :: list_nil)
  val () = expect
    (f_mul (
      f_add (f_var "A", f_var "B"),
      f_add (f_var "C", f_var "D")),
    e4)

  val () = println!("test D0")
  val e4_0 = script_present_result (init, Centry hole :: Center Lif :: Center (Lvar "x") :: list_nil)
  val () = expect (f_if (f_var "x", f_hole (), f_hole ()), e4_0)

  val () = println!("test D1")
  // "if a then b else c"
  val e4_1 = script_present_result (init,
    Centry hole :: Center Lif :: Center (Lvar "A") :: Center (Lvar "B") :: Center (Lvar "C") :: list_nil)
  val () = expect (f_if (f_var "A", f_var "B", f_var "C"), e4_1)

  val () = println!("test E")
  // (if a then b else c) + d
  val e5 = script_present_result (init,
    Centry hole :: Clparen :: Center Lif :: Center (Lvar "A") :: Center (Lvar "B") :: Center (Lvar "C") :: Crparen :: Center Ladd :: Center (Lvar "D") :: list_nil)
  val () = expect (
    f_add (
      f_if (
        f_var "A",
        f_var "B",
        f_var "C"
      ),
      f_var "D"),
    e5)

  // \ab+c*d
  val () = println!("test F")
  val e6 = script_present_result (init,
      Centry hole :: Center Llam :: Center (Lvar "A") :: Center (Lvar "B") :: Center Ladd :: Center (Lvar "C") :: Center Lmul :: Center (Lvar "D") :: list_nil)
  val () = expect (
    f_lam (f_var "A", f_add (f_var "B", f_mul (f_var "C", f_var "D"))),
    e6)

  // a + b * d : t
  val () = println!("test G")
  val e7 = script_present_result (init,
      Centry hole :: Center (Lvar "A") :: Center Ladd :: Center (Lvar "B") :: Center Lmul :: Center (Lvar "D") :: Center Lannot :: Center (Lvar "T") :: list_nil)
  val () = expect (
    f_annot (f_add (f_var "A", f_mul (f_var "B", f_var "D")), f_var "T"),
    e7)

  val () = println!("test fun-app")
  val e7 = script_present_result (init,
    Centry hole :: Center (Lvar "F") :: Center Lapp :: Center (Lvar "X") :: Center Lapp :: Center (Lvar "Y") :: list_nil)
  val () = expect (
    f_app (f_app (f_var "F", f_var "X"), f_var "Y"),
    e7)
}
(*
real languages?
- freeform comment nodes
- begin end blocks (or the like, e.g. { } blocks)
- different kind of brackets

some languages also have variadic constructs
- e.g. f (x, y)
- lists, arrays, etc. (literals), e.g. [a,b,c,d]
  - this is same as with fun-app, actually
- how do we handle fun-app ???
 (a b) --> (a Apply b)
 fun-app associates to the left:
 f x y z ==> ((f x) y) z
some languages also have different constructs
- let|local-in-end
- E where E1
- fun|val a = b  (declaration list)

ATS to JS?

undo?

what's a fun real language? say regexes? or relational algebra

One of the salient features of the model is the representation of a selection as a sequence of
context layers paired with the selected subtree. This representation makes it easy to reason about
selections – and as an added bonus its Haskell implementation is quite efficient. Moreover it can
straightforwardly be generalised to support a style of selection in which a number of adjacent
siblings are simultaneously selected. This style of selection is necessary when editing structures
which use variadic nodes to represent trees formed of semantically associative operators – such
as sequential composition (of commands in a programming language), and juxtaposition (of
words, paragraphs, etc. in a natural language).
- what is this style of selection?
- what if you had multiple trees under selection/focus AT ONCE? e.g. a list of trees
- you could also make a separate MODE for that. so if user enters BEGIN, say
  - the editor starts the mode where it will

A further layer of design which we hinted at earlier introduces a scratchpad and cut buffer,
and supports more radical structural changes and better undoability properties. The details are
straighforward and only space considerations led us to exclude them.

Finally, we have ignored user-interface issues related to input of commands, to layout of the
visible representation of the tree, and to navigation and selection using mouse gestures. We
intend to pursue all these in a subsequent paper in the same constructive mathematical style.
*)
//
(* ****** ****** *)
//
val () = test_html()
val () = main()
//
(* ****** ****** *)

%{$

function
JSarray_clear (arr) {
    arr.length = 0;
}

function
escape_html_replace(m) {
    switch (m) {
    case '&':
        return '&amp;';
    case '<':
        return '&lt;';
    case '"':
        return '&quot;';
    default:
        return '&#039;';
    }
}
function
escape_html (unsafe)
{
  return unsafe.replace(new RegExp("[&<\"']", "g"), escape_html_replace);
}
//
ats2jspre_the_print_store_clear();
my_dynload();
alert(ats2jspre_the_print_store_join());
//
%} // end of [%{$]
