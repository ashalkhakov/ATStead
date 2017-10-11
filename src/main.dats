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

#staload "./render.sats"

local

in

#include "./render.dats"

end

(* ****** ****** *)

extern
castfn ident2ID : ident -> ID
extern
castfn ID2ident : ID -> ident

(* ****** ****** *)
//
extern
fun
hello(): void = "mac#"
implement
hello() = let
  #define :: list_vt_cons
  #define nil list_vt_nil
  
  fun tests(): void = {

  // construction test
  val () = {
    // build a tree & iterate over children
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val b = tree_atom(Lvar "b")
    val x = tree_fork(Lvar "a", c :: z :: b :: nil)

    // FIXME: how to send ATS closures to JS code?
    val () = println!("children foreach, starting at root")
    val () = children_foreach (x, lam x =<cloref1> println!(label(x))) // c, z, b
    val () = println!("preorder foreach, starting at root")
    val () = preorder_foreach (x, lam x => println!(x)) // a, c, z, b    
   
    val () = tree_delete x
  }
  // preorder test
  val () = {
    val () = println!("preorder test:")
    val c = tree_fork(Lvar"a", tree_fork(Lvar"b", tree_atom(Lvar"c") :: nil) :: tree_fork(Lvar"d", tree_atom(Lvar"e") :: nil) :: nil)
    val () = preorder_foreach (c, lam x => println!(x)) // a, b, c, d, e
    val () = tree_delete c
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

  // entry with precedence test 1
  val () = {
    val () = println!("simple entry test 1")
    
    val x = hole()
    val st = root(x)
    val st = entry (Lvar"a", st)
    val st = entry (Ladd(), st)
    //val st = entry (Lvar"b", st)
    
    val x = host(st)
    // result should be the correct tree: mul(var"a",hole()(*var"b"*))
    val () = preorder_foreach(x, lam x => println!(x))
    val () = tree_delete(x)
  }
  // entry with precedence test 2
  val () = {
    val () = println!("simple entry test 2")

    // start with empty tree
    val x = hole()     
    val st = root(x)
    // enter var"a", mul(), var"b", add(),var"c",mul(),var"d"
    val st = entry (Lvar"a", st)
    val st = entry (Lmul(), st)
    val st = entry (Lvar"b", st)
    val st = entry (Ladd(), st)
    val st = entry (Lvar "c", st)
    val st = entry (Lmul(), st)
    val st = entry (Lvar "d", st)

    val x = host(st)
    // result should be the correct tree: add(mul(var"a",var"b"),mul(var"c",var"d"))
    val () = preorder_foreach(x, lam x => println!(x))
    val () = tree_delete(x)
  }

  } (* end of [tests] *)

  val () = tests ()
  val (pf_d | d) = dom_get_by_id (string2id"btn-run-tests")
  val-true = dom_is_some (d)
  val () = dom_add_listener (d, "click", lam (d, e) => tests (), false)
  val () = dom_putback (pf_d | d)
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
  val d = append_mac(element("div"), attrib_mac(element("span"), "style", "border:1px solid", "id", "MY-caret"))
 
  val xclo = "hey"
  val () = d.add_listener ("click", lam (d0 : !dom1, e : !event): void =<cloref1> {
    val-true = dom_is_some d0
    val () = alert("caret clicked! and we also have: " + xclo)
    val style = dom_get_attribute (d0, "style")
    val b0 = (g0ofg1)"font-weight:bold"
    val s = (if style = b0 then "" else "font-weight:bold")
    val () = dom_set_attribute (d0, "style", s)
  }, false)

  val () = d["class"] := "foobar"
  val d = append_mac (d, text("Hello!"), text(" "), append_mac(element("span"), text("there")), text("!"))
  
  val d = append_mac (d, append_mac (element("p"), text("Please check the box")), let
      val inp = attrib_mac (element("input"), "type", "checkbox", "id", "my-checkbox")
      val () = inp.add_listener("click", lam (inp : !dom1, evt : !event): void =<cloref1> evt.prevent_default(), false)
    in
      inp
    end)

  val (pf | p) = dom_get_by_id (string2id("sdom-test"))
  val-true = dom_is_some (p)
  val () = dom_append_child (p, d)
  val () = dom_putback (pf | p)
in

end
//
(* ****** ****** *)
//
datatype navtype =
    | NTleft
    | NTright
    | NTup
    | NTdown
    | NTfirst
    | NTlast
    | NTtop
//
datavtype
command =
  | CMDentry of tree // begin with this tree
  | CMDnav of navtype // navigate the tree
  | CMDreplace of tree // replace focussed subtree with the new tree
  | CMDinsert of label // insert a new label
  | CMDenter of label // enter a new label, taking precedence into account
  | CMDopen of () // opening paren
  | CMDclose of () // closing paren
  | CMDnop // no action!
//
extern
fun
present (subtree1, command): subtree1
//
implement
present (focussed, cmd) =
(
case+ cmd of
| ~CMDentry t => let
    val ot = flatten focussed
    val ot = host ot
    val () = tree_delete (ot)
    val r = root t
    val r = unflatten r
  in
    r
  end
| ~CMDnav nt => let
  in
    case+ nt of
    | NTleft () => left1 focussed
    | NTright () => right1 focussed
    | NTup () => up1 focussed
    | NTdown () => down1 focussed
    | NTfirst () => first_child1 focussed
    | NTlast () => last_child1 focussed
    | NTtop () => back_to_top1 focussed
  end
| ~CMDreplace t => replace1 (t, focussed)
| ~CMDinsert lab => insert1 (lab, focussed)
| ~CMDenter lab =>  entry1 (lab, focussed)
| ~CMDopen () => open (focussed)
| ~CMDclose () => close (focussed)
| ~CMDnop () => focussed
)
//
extern
fun
input_events (subtree1, (subtree1, string) -> subtree1): void = "mac#"
//
extern
fun
hello_sdom_tree (): void = "mac#"
implement
hello_sdom_tree (): void = let
  #define :: list_vt_cons
  #define nil list_vt_nil
  
  extern
  castfn
  string2ID : string -> ID  

  // construct stuff!
  val x = hole ()
  
  fun
  action (s: subtree1, evt: string): subtree1 = let
    // turn the input event into a command for the subtree
    (*
     * need some kind of parsing here... this gets tedious fast.
     * e.g. entering [a-zA-Z][a-zA-Z0-9]* --> variable, save its name
     * entering ctrl-p, ctrl-n or arrow up, arrow down --> navigate
     * or even, if it's a non-shortcut, let some code decide what was entered, by lexing it!
    *)
    val cmd = (
      ifcase
      | evt = "ArrowLeft" => CMDnav (NTleft())
      | evt = "ArrowRight" => CMDnav (NTright())
      | evt = "ArrowUp" => CMDnav (NTup())
      | evt = "ArrowDown" => CMDnav (NTdown())
      | evt = "KeyP" => CMDenter (Ladd ())
      | evt = "KeyM" => CMDenter (Lmul ())
      | evt = "KeyI" => CMDenter (Lif ())
      | evt = "KeyV" => CMDenter (Lvar "MYVAR")
      | evt = "KeyH" => CMDreplace (hole ())
      | evt = "Digit1" => CMDenter (Lconst 1)
      | evt = "Digit2" => CMDenter (Lconst 2)
      | evt = "Digit3" => CMDenter (Lconst 3)
      | evt = "Digit9" => CMDopen ()
      | evt = "Digit0" => CMDclose ()
      | evt = "KeyL" => CMDenter (Llam ())
      | evt = "Space" => CMDenter (Lapp ())
      | _ => CMDnop ()
    ) : command (* end of [val] *)
    // and apply it
    val s = present (s, cmd)
    // next, re-render the state from scratch (for now)
    extern
    castfn
    string2ID : string -> ID
    val () = dom_clear_at (string2ID "container")
    val () = render (string2ID "container", s)
  in
    s
  end
  
  // run the stream!
  val rt = root(x)
  val rt = unflatten(rt)
  val () = input_events (rt, action)
in
end
//
(* ****** ****** *)
//
val () = hello()
val () = hello_sdom()
val () = hello_sdom_tree()
//
(* ****** ****** *)

%{$

function
input_events (state, handler) {
  document.onkeydown = function(evt) {
     state = handler(state, evt.code);
  };
  // draw it the first time
  state = handler(state, "");
}

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
