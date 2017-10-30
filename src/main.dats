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
//
extern
castfn ident2ID : ident -> ID
extern
castfn ID2ident : ID -> ident
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
//
ats2jspre_the_print_store_clear();
my_dynload();
alert(ats2jspre_the_print_store_join());
//
%} // end of [%{$]
