
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
    val () = println!("entry with label: ", l)
    val st = reduce (l, st)
in
    enter (l, st)
end

implement
reduce (l, st) =
    if irreducible (l, st) then (println!("irreducible");st)
    else let
        val () = println!("moving up, retrying")
        val st = up st
    in
        reduce (l, st)
    end

(*
implement
irreducible (l, st) =
    at_hole st || topmost st || ~rightmost st ||
        ~producable (l, selected_label (up st))
*)

implement
producable (op2, op1) =
  (op1 = op2 && assl op1) || prec op1 > prec op2
  
(* ****** ****** *)

implement
enter1 (l, st) = lift (st, lam st =<cloref1> enter (l, st))
implement
entry1 (l, st) = lift (st, lam st =<cloref1> entry (l, st))
implement
left1 (st) = lift (st, lam st =<cloref1> left st)
implement
first_child1 (st) = lift (st, lam st =<cloref1> first_child st)
implement
right1 (st) = lift (st, lam st =<cloref1> right st)
implement
last_child1 (st) = lift (st, lam st =<cloref1> last_child st)
implement
up1 (st) = lift (st, lam st =<cloref1> up st)
implement
down1 (st) = lift (st, lam st =<cloref1> down st)
implement
back_to_top1 (st) = lift (st, lam st =<cloref1> back_to_top st)
implement
insert1 (l, st) = lift (st, lam st =<cloref1> insert (l, st))
(*
implement
replace1 (t, st) = lift (st, lam st =<cloref1> replace (t, st))
*)

(* ****** ****** *)

%{$
//
function
eq_ident_ident (x, y) { return x === y; }
//
var the_tree_ident = 0;
function
the_tree_ident_getnext () {
  the_tree_ident--;
  return "tid-" + the_tree_ident;
}
var the_tree_allocs = {};
//
function
tree_atom(x) {
  var i = the_tree_ident_getnext();
  var res = { prev: null, next: null, value: x, parent: null, child : null, ident: i };
  the_tree_allocs[i] = res;
  return res;
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
  delete the_tree_allocs[x.ident];
}

function
tree_preorder_foreach(x, f) {
  if (x === null)
    return;

  var r = x;
  
  var node = r
  while (true) {
    f(node.value);
    if (node.child !== null) {
       node = node.child // walk down
    } else {
       while (node.next === null) {
         if (node === r)
           return;
         node = node.parent; // walk up
       }
       node = node.next; // ... and right
    }
  }
}

function
tree_label(x) {
  return x.value;
}
function
tree_ident(x) {
  return x.ident;
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
    f[0](f, c); // clofun
    c = c.next;
  }
}

//---- subtree
function
subtree_root(x) {
  return x;
}
function
subtree_foreach_parent(x, epid, f) {
  var xid = tree_ident(x);
  var r = subtree_back_to_top(x);
  
  var node = r
  while (true) {
    var pid;
    if (node.parent === null) {
      if (epid === null) {
        pid = null;
      } else {
        pid = epid;
      }
    } else {
      var parent_id = tree_ident(node.parent);
      pid = [parent_id];
    }

    f[0](f, node, xid, pid);
    if (node.child !== null) {
       node = node.child // walk down
    } else {
       while (node.next === null) {
         if (node === r)
           return;
         node = node.parent; // walk up
       }
       node = node.next; // ... and right
    }
  }
}
function
subtree_foreach(x, f) {
  subtree_foreach_parent(x, null, f);
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
subtree_swap(tree, st) {
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
    // the current focus is replaced with [t]
    var sel = st;
    var p = st.parent;
    if (p !== null) {
      if (p.child == st) {
        p.child = t;
      }
    }
    t.parent = p;
    st.parent = null;

    if (st.prev !== null) {
      st.prev.next = t;
    }
    t.prev = st.prev;
    st.prev = null;

    if (st.next !== null) {
      st.next.prev = t;
    }
    t.next = st.next;
    st.next = null;

    // navigate down the new subtree
    st = subtree_down(t);
    // and put the extracted tree back
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
    var res = subtree_at_hole(st)
    || subtree_topmost(st)
    || !subtree_rightmost(st)
    || !producable (l, tree_label (subtree_up(st)));
    return res;
}
//----------------------
// subtree1

/*
what's the new approach?
- state: stack of trees (stack of holes, even)
  - conceptually, extends from the left rightwards
  - every element E has some element on its "left", say P (except the root)
    - precondition: P is a hole!
    - then P is the parent of parenthesized subtree E
- last element of stack: just the current focussed subtree :)
  - other elements (the path): these are all HOLES!
- open paren:
  - if current head is a hole, do nothing
    but if it's not, then make a fresh hole H and swap it with current head
  - push to the end of stack (? like, just put it after current head, makes it into a queue???)
- close paren:
  - if there is only one element on stack, ignore
  - otherwise, pop the node N off the stack, and replace the hole (previous element of stack)
    with N (destroying the hole in the process)
- rendering
  - get the first element of stack, the root
  - render the nodes recursively
    - check the stack, if you have a node to look for... then wait for it
      - if current node id's same as the node we are looking for, begin rendering the subtree!
    - otherwise, you have no nodes to look for, just render stuff
*/
function
subtree1_unflatten(s) {
  var h = tree_hole ();
  return {stk: [h], sel: s};
}
function
subtree1_flatten(st) {
  var focus = st.sel;
  var stk = st.stk;
  while (stk.length > 0) {
    var p = stk.pop();
    focus = subtree_back_to_top(focus);
    focus = subtree_replace(p, focus);
  }
  return focus;
}
function
subtree1_lift(st,f) {
  var sel = st.sel;
  sel = f[0](f, sel);
  st.sel = sel;
  return st;
}
function
subtree1_replace(t, st) {
  var clo = [function(cenv,st) {
    return subtree_replace(cenv[1], st);
  }, t];
  return subtree1_lift(st, clo);
}
function
subtree1_open(st) {
  var sel = st.sel;

  var h = tree_hole ();
  st.stk.push(sel);
  st.sel = h;

  return st;
}
function
subtree1_close(st) {
  // pop the most-recent subtree and incorporate it back into its parent
  var stk = st.stk;
  var sel = st.sel;  
  if (stk.length > 1) {
    var n = stk.pop();
    sel = subtree_back_to_top(sel);
    n = subtree_replace(sel, n);
    st.sel = n;
    return subtree1_right (st);
  } else {
    return st;
  }
}
function
subtree1_foreach(st, f) {
  var stk = st.stk;
  var i = 0;
  while (i < stk.length) {
    var n = stk[i];
    var np = i>0? stk[i-1] : null;
    var n_id = np? [tree_ident(np)] : null;
    subtree_foreach_parent(n, n_id, f);
    i++;
  }
  var last = stk[stk.length-1];
  var n_id = tree_ident(last);
  n_id = [n_id];
  subtree_foreach_parent(st.sel, n_id, f);
}

%} // end of [%{$]

