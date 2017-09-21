//
(* ****** ****** *)

%{$

//
function
dom_is_some(d) { return d !== null; }
function
dom_is_none(d) { return d === null; }
//
function
dom_create_element (x) {
  return document.createElement(x);
}
function
dom_add_listener (x, evt, f, usecapture) {
  x.addEventListener(evt, function(e) {
    return f[0](f, this, e);
  }, usecapture);
}
/*
function
dom_remove_listener (x, evt, f, usecapture) {
  x.removeEventListener(evt, f, usecapture);
}
*/
function
event_prevent_default (e) {
  e.preventDefault();
}
function
event_stop_propagation (e) {
  e.stopPropagation();
}
function
dom_set_attribute (x, k, v) {
  x.setAttribute(k, v);
}
function
dom_get_attribute (x, k) {
  return x.getAttribute(k);
}
function
dom_create_text (v) {
  return document.createTextNode(v);
}

var __the_id__ = 0;
function
get_next_id() {
  __the_id__--;
  return "sdom" + __the_id__;
}
function
dom_get_id (x) {
  return x.id === ""? [] : [x.id];
}

function
dom_get_by_id (id) {
  return document.getElementById(id);
}
function
dom_putback(d) {
  // nop
}
function
dom_remove(d) {
  d.parentNode.removeChild(d);
}
function
dom_replace(x, y) {
  d.parentNode.replaceChild(y, x);
}
function
dom_free (d) {
// nop...
}
function
dom_append_child (p, c) {
  p.appendChild(c);
}
function
dom_insert_at (id, p) {
  var elt = document.getElementById(id);
  elt.appendChild(p);
}

%}