(* ****** ****** *)
//
absvtype dom (l:addr) = ptr
absvtype event = ptr
vtypedef dom1 = [l:agz] dom(l)
vtypedef dom0 = [l:addr] dom(l)

typedef event_handler = (!dom1, !event) -<cloref1> void

fun
dom_is_some {l:addr} (!dom(l)): bool (l>null) = "mac#dom_is_some"
fun
dom_is_none {l:addr} (!dom(l)): bool (l==null) = "mac#dom_is_none"

fun
dom_create_element (string): dom1 = "mac#"
overload element with dom_create_element

fun
dom_add_listener (!dom1, string(*type*), event_handler, bool(*use capture*)): void = "mac#"
overload .add_listener with dom_add_listener
(*
fun
dom_remove_listener (!dom1, string(*type*), event_handler, bool(*use_capture*)): void = "mac#"
overload .remove_listener with dom_remove_listener
*)
fun
event_prevent_default (!event): void = "mac#"
overload .prevent_default with event_prevent_default
fun
event_stop_propagation (!event): void = "mac#"
overload .stop_propagation with event_stop_propagation

fun
dom_set_attribute (!dom1, string, string): void = "mac#"
overload [] with dom_set_attribute

fun
dom_get_attribute (!dom1, string): string = "mac#"
overload [] with dom_get_attribute

fun
dom_create_text (string): dom1 = "mac#"
overload text with dom_create_text

abstype ID
fun
eq_id_id (ID, ID): bool = "ext#ATSCKpat_string"
overload = with eq_id_id
castfn
string2id (string): ID
castfn
id2string (ID): string
fun
get_next_id (): ID = "mac#"

fun
dom_get_id (!dom1): Option_vt(ID) = "mac#"

absview dom_parent_v (l:addr) // has parent
praxi
dom_parent_v_free : dom_parent_v(null) -<prf> void
fun
dom_get_by_id (ID): [l:addr] (dom_parent_v(l) | dom(l)) = "mac#" // still linked to its parent
fun
dom_putback {l:addr} (dom_parent_v(l) | dom(l)): void = "mac#" // identity at runtime
fun
dom_remove {l:agz} (dom_parent_v(l) | !dom(l)): void = "mac#" // unlink from parent
fun
dom_replace {l:agz} (dom_parent_v(l) | x: !dom(l), y: dom1): void = "mac#" // replace [x] with [y]

fun
dom_free (dom0): void = "mac#"

fun
dom_append_child (!dom1, dom1): void = "mac#"
overload .append with dom_append_child

fun
dom_insert_at (ID, dom1): void = "mac#"

(* ****** ****** *)

local

macrodef
rec
auxlist
  (a, xs, y) =
(
//
if iscons! (xs) then
  `(let val tmp = ,(car! xs) in dom_append_child (,(a), tmp); ,(auxlist (a, cdr! xs, y)) end)
else y // end of [if]
//
) // end of [auxlist]

macrodef
rec
auxlist2
  (a, xs, y) =
(
//
if iscons! (xs) then
  `(let
      val tmp0 = ,(car! xs)
      val tmp1 = ,(car! (cdr! xs))
    in
      dom_set_attribute (,(a), tmp0, tmp1);
      ,(auxlist2 (a, cdr! (cdr! xs), y))
    end)
else y // end of [if]
//
) // end of [auxlist2]

in (* in of [local] *)

macdef
append_mac(x, y) =
,(
  if islist! (y) then `(let
    val tmp = ,(x)
  in
    ,(auxlist (`(tmp), y, `(tmp)))
  end) else `(let
    val tmp = ,(x)
    val chld = ,(y)
  in
    dom_append_child (tmp, chld);
    tmp
  end)
) // end of [print_mac]

macdef
attrib_mac(x, y) =
,(
  if islist! (y) then `(let
    val tmp = ,(x)
  in
    ,(auxlist2 (`(tmp), y, `(tmp)))
  end) else `(let
    val tmp = ,(x)
  in
    tmp
  end)
)

end // end of [local]
