
implement
rightup st =
    if topmost st || ~rightmost st then right st
    else let
        val st = up st
    in
        rightup st
    end

implement
next st =
    if bottommost st then
        rightup st
    else
        down st

(* ****** ****** *)
(*
replace (selected st) (replace s st) = st
*)
implement
insert (lab, st) = let
    val r = template lab
    val st' = treeinsert (r, st)
in
  st'
end

(*
selection(treeinsert t st) = selection st, if not (at_hole st)
*)

implement
kill st = replace (hole (), st)

(*
kill (treeinsert t st) = st, if atHole st
promote (treeinsert t st) = st, if not (atHole st) && not(atomic t)
id (treeinsert t st) = st, if not (atHole st) && atomic t
replace (selected st) (kill st) = st
*)

(*
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
*)

(* ****** ****** *)

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

(*
implement
irreducible (l, st) =
    at_hole st || topmost st || ~rightmost st ||
        ~producable (l, label_selected (up st))
*)

implement
producable (op2, op1) =
  (op1 = op2 && assl op1) || prec op1 > prec op2

%{$
//
function
tree_atom(x) {
  return { prev: null, next: null, value: x, parent: null, child : null }
}
function
tree_insert_before(x, y) {
  y.parent = x;
  var c = x.child;
  if (c === null) {
    x.child = y;
  } else {
    y.prev = c.prev;
    if (y.prev !== null)
      y.prev.next = y;
    c.prev = y;
    y.next = c;
    x.child = y;
  }
  return x;
}
function
tree_insert_after(x,y) {
  y.parent = x;
  var c = x.child;
  if (c === null) {
    x.child = y;
  } else {
    y.next = c.next;
    if (y.next !== null)
      y.next.prev = y;
    c.next = y;
    y.prev = c;

    //x.child = y; // preserve the current child of x
  }
  return x;
}
function
tree_fork(v, xs) {
  var t = tree_atom(v);
  var lst = ats2jspre_list_vt_reverse(xs);
  while (lst !== null) {
    var x = lst[0];
    lst = lst[1];
    tree_insert_before(t, x);
  }
  return t;
}
function
tree_delete(x) {
  // NOTE: we only allow deleting the whole tree
  // so that means we don't have to do anything here
}

function
tree_preorder_foreach(x, f) {
  if (x === null)
    return;
  var stack = [];

  stack.push(x);
  while (stack.length > 0) {
    var cur = stack.shift();
    f(cur.value);
    var c = cur.child;
    // visit children, if any
    // NOTE: we don't actually mandate that c.child.prev === null!
    // but in a good tree, it will be!
    while (c !== null) {
      stack.push(c);
      c = c.next;
    }
  }
}

function
tree_label(x) {
  return x.value;
}
function
tree_atomic(x) {
  return x.child === null;
}
function
tree_is_fork(x) {
  return x.child !== null;
}

function
tree_children_foreach(x, f) {
  var c = x.child;
  while (c !== null) {
    f(c);
    c = c.next;
  }
}

//---- subtree
function
subtree_root(x) {
  return x;
}

// - top/topmost(parent=null)
function
subtree_topmost(x) {
  return x.parent === null;
}
function
subtree_up(x) {
  if (subtree_topmost(x)) return x;
  else return x.parent;
}
// - left/leftmost(prev=null)
function
subtree_leftmost(x) {
  return x.prev === null;
}
function
subtree_left(x) {
  if (subtree_leftmost(x)) return x;
  else return x.prev;
}
// - right/rightmost(next=null)
function
subtree_rightmost(x) {
  return x.next === null;
}
function
subtree_right(x) {
  if (subtree_rightmost(x)) return x;
  else return x.next;
}
// - bottom/bottommost(child=null)
function
subtree_bottommost(x) {
  return x.child === null;
}
function
subtree_down(x) {
  if (subtree_bottommost(x)) return x;
  else return x.child;
}

function
subtree_first_child(x) {
  while (!subtree_leftmost(x)) {
    x = subtree_left(x);
  }
  return x;
}
function
subtree_last_child(x) {
  while (!subtree_rightmost(x)) {
    x = subtree_right(x);
  }
  return x;
}
function
subtree_back_to_top(x) {
  while (!subtree_topmost(x)) {
    x = subtree_up(x);
  }
  return x;
}
function
subtree_next_such_that(p, st) {
  var st0 = subtree_next(st);
  var st1 = st0;
  while (true) {
    if (subtree_topmost(st1) || p(st1)) break;
    else st1 = subtree_next(st1);
  }
  if (p(st1)) return st1;
  return st;
}

//----------------------
// subtree modification

function
subtree_replace(tree, st) {
  tree.parent = st.parent;
  tree.next = st.next;
  tree.prev = st.prev;

  if (st.next !== null) {
    st.next.prev = tree;
  }
  if (st.prev !== null) {
    st.prev.next = tree;
  }
  var p = st.parent;
  if (p !== null) {
    if (p.child === st) {
      p.child = tree;
    }
  }
 
  st.next = null;
  st.prev = null;
  st.parent = null;
  tree_delete(st);

  return tree;
}
function
subtree_unlink(st) {
  var p = st.parent;
  //if (p !== null) // assume non-null
  {
    if (p.child === st) {
      p.child = st.next;
    }
    st.parent = null;
  }
  if (st.next !== null) {
    st.next.prev = st.prev;
    st.next = null;
  }
  if (st.prev !== null) {
    st.prev.next = st.next;
    st.prev = null;
  }
  return st;
}
function
subtree_at_hole(st) {
  var lab = tree_label(st);
  return label_is_hole(lab) && tree_atomic(st);
}
function
subtree_treeinsert(t, st) {
  if (subtree_at_hole(st)) {
    return subtree_replace(t, st);
  } else {
    var p = st.parent;
    if (p !== null) {
      if (p.child == st) {
        p.child = t;
      }
    }
    t.parent = p;

    if (st.prev !== null) {
      st.prev.next = t;
    }
    t.prev = st.prev;

    if (st.next !== null) {
      st.next.prev = t;
    }
    t.next = st.next;

    var sel = t;
    st = subtree_down(st);
    st = subtree_replace(sel, st);

    return st;
  }
}
function
subtree_promote(st) {
  var p = st.parent;

  if (p === null)
    return st;

  var sel = subtree_unlink(st);
  return subtree_replace(sel, p);
}

//----------------------
function
irreducible(l, st) {
    return subtree_at_hole(st)
    || subtree_topmost(st)
    || !subtree_rightmost(st)
    || !producable (l, label_selected (subtree_up(st)));
}
//----------------------
// subtree1

// TODO

%} // end of [%{$]

