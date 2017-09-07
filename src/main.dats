(*
** Hello, world!
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
"{$LIBATSCC2JS}/SATS/list_vt.sats"

(* ****** ****** *)

#define ATS_MAINATSFLAG 1
#define ATS_DYNLOADNAME "my_dynload"

(* ****** ****** *)

#staload "./sdom.sats"
local
#include "./sdom.dats"
in
end

(* ****** ****** *)

#staload "./tree.sats"

local

in

#include "./label.dats"
#include "./tree.dats"

end

(* ****** ****** *)
(*
absvtype subtree' = ptr
(*
extern
val
unflatten : subtree -> subtree' = "mac#subtree1_unflatten"
extern
val
flatten : subtree' -> subtree = "mac#subtree1_flatten"
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
*)
*)

(* ****** ****** *)
//
extern
fun
hello(): void = "mac#"
implement
hello() = let
  #define :: list_vt_cons
  #define nil list_vt_nil

  // construction test
  val () = {
    // build a tree & iterate over children
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val b = tree_atom(Lvar "b")
    val x = tree_fork(Lvar "a", c :: z :: b :: nil)

    // FIXME: how to send ATS closures to JS code?
    val () = children_foreach (x, lam x => println!(label(x)))
    val () = preorder_foreach (x, lam x => println!(x))
   
    val () = tree_delete x
  }

  // navigation test
  val () = {
    // build a tree & iterate over children
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val b = tree_atom(Lvar "b")
    val x = tree_fork(Lvar "a", c :: z :: b :: nil)

    val st = root(x)
    val st = down(st)
    val st = right(st)
    // should give [z]:
    val () = println!("root, down, right: ", selected_label(st))// this is what selected is for!
    val st = left(st)
    // should give [c]:
    val () = println!("now left: ", selected_label(st))
    val x = host(st)
    // should give [a]:
    val () = println!("host: ", label(x))
 
    val () = tree_delete(x)
  }

  // replace test
  val () = {
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val x = tree_fork(Lvar "a", c :: z :: nil)
   
    val () = println!("before replacing:")
    // a, c, z:
    val () = preorder_foreach (x, lam x => println!(x))
   
    val st = root(x)
    val st = down(st)
    val () = println!("looking at ", selected_label(st))
    val c1 = tree_atom(Lvar "c1")
    val () = println!("new tree: ", label(c1))
    val st = replace(c1, st)
    val x = host(st)
   
    val () = println!("after replacing [c] with [c1]:")
    // a, c1, z:
    val () = preorder_foreach (x, lam x => println!(x))
   
    val st = root(x)
    val a = tree_atom(Lvar "A")
    val st = replace(a, st)
    val x = host(st)

    val () = println!("after replacing root with [A]:")
    // A:
    val () = preorder_foreach (x, lam x => println!(x))

    val () = tree_delete(x)
  }

  // promote test
  val () = {
    val c = tree_atom(Lvar "c")
    val z = tree_fork(Lvar "z", c :: nil)
    val x = tree_fork(Lvar "a", z :: nil)
   
    val () = println!("before promoting [c] to [z]:")
    // a, z, c
    val () = preorder_foreach (x, lam x => println!(x))

    val st = root(x)
    val st = down(st)
    val st = down(st)
    val st = promote(st)
    val x = host(st)

    val () = println!("after promoting [c] to [z]:")
    // a, c
    val () = preorder_foreach (x, lam x => println!(x))
   
    val () = tree_delete(x)
  }

  // treeinsert
  val () = {
    // insert at hole
    val () = println!("insert at hole")
    val () = {
      val x = hole()
     
      val st = root(x)
      val y = tree_atom(Lvar "A")
      val st = treeinsert (y, st)

      val x = host(st)
      // A:
      val () = preorder_foreach (x, lam x => println!(x))
     
      val () = tree_delete(x)
    }
    // insert at non-hole
    val () = println!("insert at non-hole")
    val ()  = {
      val x = tree_fork(Lvar "plus", hole() :: hole() :: nil())
     
      val st = root(x)
      val y = tree_atom(Lvar "A")
      val st = treeinsert(y, st)
     
      val x = host(st)
      // add, A, hole:
      val () = preorder_foreach(x, lam x => println!(x))
     
      val () = tree_delete(x)
    }
  }

  // result: c, z, b
  (*
  {
    var r = [];
    tree_children_foreach(x, function(c) { r.push(c.value); });
    check_assert(['c','z','b'], r, 'children traversal');
  }
  {
    var r = [];
    recur(x,function(x) { r.push(x.value); });
    check_assert(['a','c','z','b'], r, 'pre-order traversal');
  }
  //console.log(tree_right(tree_bottom(x)).value);
  *)

  // NEXT: navigation/subtree type
in
end
(* ****** ****** *)
//

(* ****** ****** *)
//
extern
fun
hello_sdom(): void = "mac#"
implement
hello_sdom() = let
(*
will still need some "handles" for sub-DOM-trees of a given tree.

and we'll probably require some form of "dom node sharing" to save those things
also, we will be using "lifecycle" events for tree nodes:
- tree node added --> dom node needs to be constructed following the template
(ACTUALLY... let's just do it ourselves! for every subtree in the [template] result,
produce its dom, then construct the dom node, link them up, perform UPDATE)
- tree node updated --> dom node needs to be updated
- tree node deleted --> nah, automatically deletes the corresponding dom node

e.g. If[hole,hole,hole]
and then we have:
- pointer to DOM node representing the whole expression
- for every child, pointer to DOM node representing the child
*)
  val d = element("div")
 
  val caret = element("span")
  val () = caret["style"] := "border:1px solid"
  val () = caret["id"] := "MY-caret"
  val () = d.append(caret)

  val () = d.add_listener ("click", lam (d, e) => {
    val () = alert("hello!")
    val () = d["style"] := "font-weight:bold"
  }, false)

  val () = d["class"] := "foobar"
  val () = d.append(text("Hello!"))
  val () = d.append(text(" "))
  val s = element("span")
  val () = s.append(text("there"))
  val () = d.append(s)
  
  val inp = element("input")
  val () = inp["type"] := "checkbox"
  val () = inp["id"] := "my-checkbox"
  val () = inp.add_listener("click", lam (inp, evt) => evt.prevent_default(), false)
  val inp_para = element("p")
  val () = let val txt = text("Please check the box") in inp_para.append(txt) end
  val () = d.append(inp_para)
  val () = d.append(inp)
  
  val () = dom_insert_at ("sdom-test", d)
in

end

//
(* ****** ****** *)
//
val () = hello()
val () = hello_sdom()
//
(* ****** ****** *)

%{$

//----------------------

// from https://stackoverflow.com/questions/201183/how-to-determine-equality-for-two-javascript-objects
function objectEquals(x, y) {
    'use strict';

    if (x === null || x === undefined || y === null || y === undefined) { return x === y; }
    // after this just checking type of one would be enough
    if (x.constructor !== y.constructor) { return false; }
    // if they are functions, they should exactly refer to same one (because of closures)
    if (x instanceof Function) { return x === y; }
    // if they are regexps, they should exactly refer to same one (it is hard to better equality check on current ES)
    if (x instanceof RegExp) { return x === y; }
    if (x === y || x.valueOf() === y.valueOf()) { return true; }
    if (Array.isArray(x) && x.length !== y.length) { return false; }

    // if they are dates, they must had equal valueOf
    if (x instanceof Date) { return false; }

    // if they are strictly equal, they both need to be object at least
    if (!(x instanceof Object)) { return false; }
    if (!(y instanceof Object)) { return false; }

    // recursive object equality check
    var p = Object.keys(x);
    return Object.keys(y).every(function (i) { return p.indexOf(i) !== -1; }) &&
        p.every(function (i) { return objectEquals(x[i], y[i]); });
}
function check_assert(x, y, msg) {
  if (!objectEquals(x,y)) {
    var message = ['Error: ', msg, '\nExpected:', JSON.serialize(x), '\nGot:', JSON.serialize(y)].join();
    throw message;
  }
  else
    console.log('OK: ' + msg);
}
//
ats2jspre_the_print_store_clear();
my_dynload();
alert(ats2jspre_the_print_store_join());
//
%} // end of [%{$]
