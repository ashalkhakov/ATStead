staload "./sdom.sats"
staload "./tree.sats"

extern
castfn
ident2ID : ident -> ID

//
// TODO: what else do we need during update/create?
// - selected subtree or not? under focus or not?
implement
update (id, d, lab) = let
  // TODO: new function, just replace the only text child!
  val t = text(" OH HAI")
  val () = dom_append_child (d, t)
in
end

extern
fun
render_label (label): string = "mac#"
implement
render_label (lab) =
    case+ lab of
    | Lhole() => "[]"
    | Lif () => "if"
    | Ladd () => "add"
    | Lmul () => "mul"
    | Lvar s => "var(" + s + ")"
    | Lconst i => "const()" // oh my, convert int -> string
    | Llam () => "lam"
    | Lannot() => "annot"
    | Lapp () => "app"

implement
create (id, lab) = let
  (*
    depends on fixity:
    - assl
    - prec
    - label_is_hole
    
    horizontal or vertical layout
    - horizontal layot:
      - infix? 
  *)
  val txt = render_label lab
  val d =
    attrib_mac(
      append_mac(element("span"), text(txt)),
      "id", id2string(id),
      "class", "spaced"
    ) (* end of [val] *)
  // add a handler too
  val () = dom_add_listener (
      d
    , "click"
    , lam (d, evt) =<cloref1> let
        val () = alert ("clicked on: " + id2string id + "\ncontaining " + txt)
        val () = evt.stop_propagation ()
      in
      end
    , false
    ) (* end of [val] *)
  // end of [val]
in
  d
end

// render the tree starting at the given subtree
implement
render (pid, node) = let
  val id = selected_ident (node)
  val dom_id = ident2ID id
  val lab = selected_label node
  val dm = create (dom_id, lab)
  val () = dom_insert_at (pid, dm)
  val () = selected_children_foreach (
    node
  , lam chld =<cloref1> render (dom_id, chld)
  ) (* end of [val] *)
in
end
