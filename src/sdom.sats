(* ****** ****** *)
//
absvtype dom = ptr
absvtype event = ptr

typedef event_handler = (!dom, !event) -> void

fun
dom_create_element (string): dom = "mac#"
overload element with dom_create_element

fun
dom_add_listener (!dom, string(*type*), event_handler, bool(*use capture*)): void = "mac#"
overload .add_listener with dom_add_listener
(*
fun
dom_remove_listener (!dom, string(*type*), event_handler, bool(*use_capture*)): void = "mac#"
overload .remove_listener with dom_remove_listener
*)
fun
event_prevent_default (!event): void = "mac#"
overload .prevent_default with event_prevent_default
fun
event_stop_propagation (!event): void = "mac#"
overload .stop_propagation with event_stop_propagation

fun
dom_set_attribute (!dom, string, string): void = "mac#"
overload [] with dom_set_attribute

fun
dom_get_attribute (!dom, string): string = "mac#"
overload [] with dom_get_attribute

fun
dom_create_text (string): dom = "mac#"
overload text with dom_create_text

fun
dom_get_by_id (string): dom = "mac#"

fun
dom_free (dom): void = "mac#"

fun
onkeypress (!dom): void = "mac#"

fun
dom_append_child (!dom, dom): void = "mac#"
overload .append with dom_append_child

fun
dom_insert_at (string(*id of element*), dom): void = "mac#"
