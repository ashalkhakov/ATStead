#define
LIBATSCC2JS_targetloc
"$PATSHOME/contrib/libatscc2js/ATS2-0.3.2"
//
#include
"{$LIBATSCC2JS}/staloadall.hats"
//
#define ATS_MAINATSFLAG 1
#define ATS_DYNLOADNAME "my_dynload"
//
#staload "../src/sdom.sats"
local in #include "../src/sdom.dats" end
//
extern
fun
hello_sdom(): void = "mac#"
implement
hello_sdom() = let
  val d = append_mac(
    element("div"),
    attrib_mac(element("span"), "style", "border:1px solid", "id", "MY-caret"))
 
  val xclo = "hey"
  val () = d.add_listener ("click", lam (d0, e) =<cloref1> {
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
      val () = inp.add_listener("click", lam (inp, evt) =<cloref1> evt.prevent_default(), false)
    in
      inp
    end)

  val (pf | p) = dom_get_by_id (string2id("sdom-test"))
  val-true = dom_is_some (p)
  val () = dom_append_child (p, d)
  val () = dom_putback (pf | p)
in

end

val () = hello_sdom ()

%{$
my_dynload();
%}