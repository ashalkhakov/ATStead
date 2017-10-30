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
#staload "../src/tree.sats"

local

in

#include "../src/label.dats"
#include "../src/tree.dats"

end
//
extern
fun
logats_ext {a:t@ype} (a): void = "mac#logats_ext"
//
implement{a}
logats_tmp (x) = logats_ext {a} (x)
//
(* ****** ****** *)
//
extern
fun
eq_JSarr_JSarr {a:t@ype} ( (a,a)-> bool, JSarray(a), JSarray(a)): bool = "mac#eq_JSarr_JSarr"
//
extern
fun
aeq (JSarray(label), List(label)): bool
implement
aeq (arr1, lst) = let
  val arr2 = JSarray_make_list(lst)
  val all = eq_JSarr_JSarr (eq_label_label, arr1, arr2)
in
  all
end
//
(* ****** ****** *)
//
extern
fun
check_preorder (!tree, List(label)): bool
implement
check_preorder (x, expected) = let
  val A = JSarray_nil{label}()
  val () = preorder_foreach (x, lam x =<cloref1> ignoret (JSarray_push (A, x)))
in
  aeq (A, expected)
end
//
(* ****** ****** *)
//
datatype N = N of (label, JSarray(N))
//
extern
fun eq_N_N (N, N): bool = "mac#eq_N_N"
//
fun
N_leaf (x: label): N = N (x, JSarray_nil{N}())
//
fun
N_node (x: label, cs: List(N)): N = let
  val r = JSarray_make_list{N}(cs)
in
  N (x, r)
end
//
fun
N_of_tree (r: !tree): N = let
  fun F (l: label):<cloref1> N = N (l, JSarray_nil())
  fun G (x: N, r: N):<cloref1> N = let
      val+ N (l, a) = x
      val a = JSarray_push (a, r)
    in
      x
    end // end of [G]
  val n = tree_map (r, F, G)
in
  n
end
//
(* ****** ****** *)
//
extern
fun
hello(): void = "mac#"
implement
hello() = let
  #define :: list_vt_cons
  #define nil list_vt_nil
  
  val () = {
    val b = tree_atom(Lvar"b")
    val c = tree_atom(Lvar"c")
    val x = tree_fork(Lvar"a", b :: c :: nil)
    
    val n = N_of_tree (x)
    #define :: list_cons
    #define nil list_nil
    val-true = eq_N_N (N_node (Lvar"a", N_leaf (Lvar "b") :: N_leaf (Lvar "c") :: nil), n)
    val () = tree_delete (x)
  }
  val () = {
    val d = tree_atom(Lvar"d")
    val c = tree_atom(Lvar"c")
    val x = tree_fork(Lvar"a", tree_fork(Lvar"b", d :: nil) :: c :: nil)
    
    val n = N_of_tree (x)
    #define :: list_cons
    #define nil list_nil
    val-true = eq_N_N (N_node (Lvar"a", N_node (Lvar "b", N_leaf (Lvar "d") :: nil) :: N_leaf (Lvar "c") :: nil), n)
    val () = tree_delete (x)
  }
    
  val () = {
    // build a tree & iterate over children
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val b = tree_atom(Lvar "b")
    val x = tree_fork(Lvar "a", c :: tree_fork(Lvar "x", z :: nil) :: b :: nil)
    
    val n = N_of_tree (x)
    #define :: list_cons
    #define nil list_nil
    val-true =
      eq_N_N (n,
        N_node (Lvar "a",
            N_leaf (Lvar "c") ::
            N_node (Lvar "x", N_leaf (Lvar "z") :: nil) ::
            N_leaf (Lvar "b") :: nil
            )
      )
    // end of [val]
    val () = tree_delete (x)
  }

  // construction test
  val () = {
    // build a tree & iterate over children
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val b = tree_atom(Lvar "b")
    val x = tree_fork(Lvar "a", c :: z :: b :: nil)

    #define :: list_cons
    #define nil list_nil

    val () = logats("children foreach, starting at root")
    val A = JSarray_nil{label}()
    val () = children_foreach (x, lam x =<cloref1> ignoret (JSarray_push (A, label(x))))
    val-true = aeq (A, (Lvar "c" :: Lvar "z" :: Lvar "b" :: nil))
    val () = logats("preorder foreach, starting at root")
    val-true = check_preorder (x, Lvar "a" :: Lvar "c" :: Lvar "z" :: Lvar "b" :: nil)
   
    val () = tree_delete x
  }

  // preorder test
  val () = {
    val () = logats("preorder test:")
    val c = tree_fork(Lvar"a", tree_fork(Lvar"b", tree_atom(Lvar"c") :: nil) :: tree_fork(Lvar"d", tree_atom(Lvar"e") :: nil) :: nil)
    #define :: list_cons
    #define nil list_nil
    val-true = check_preorder (c, (Lvar"a" :: Lvar"b" :: Lvar"c" :: Lvar"d" :: Lvar"e" :: nil))
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
    val lab = selected_label(st)
    val () = logats("root, down, right: ", lab)// this is what selected is for!
    val-true = (lab = Lvar"z")
    val st = left(st)
    // should give [c]:
    val lab = selected_label(st)
    val () = logats("now left: ", lab)
    val-true = (lab = Lvar"c")
    val x = host(st)
    // should give [a]:
    val lab = label(x)
    val () = logats("host: ", lab)
    val-true = (lab = Lvar"a")

    val () = tree_delete(x)
  }

  // replace test
  val () = {
    val c = tree_atom(Lvar "c")
    val z = tree_atom(Lvar "z")
    val x = tree_fork(Lvar "a", c :: z :: nil)
   
    val () = logats("before replacing:")
    #define :: list_cons
    #define nil list_nil
    val-true = check_preorder (x, (Lvar"a" :: Lvar"c" :: Lvar"z" :: nil))
   
    val st = root(x)
    val st = down(st)
    val () = logats("looking at ", selected_label(st))
    val c1 = tree_atom(Lvar "c1")
    val () = logats("new tree: ", label(c1))
    val st = replace(c1, st)
    val x = host(st)
   
    val () = logats("after replacing [c] with [c1]:")
    val-true = check_preorder (x, (Lvar "a" :: Lvar "c1" :: Lvar "z" :: nil))
   
    val st = root(x)
    val a = tree_atom(Lvar "A")
    val st = replace(a, st)
    val x = host(st)

    val () = logats("after replacing root with [A]:")
    val-true = check_preorder (x, Lvar "A" :: nil)

    val () = tree_delete(x)
  }

  // promote test
  val () = {
    val c = tree_atom(Lvar "c")
    val z = tree_fork(Lvar "z", c :: nil)
    val x = tree_fork(Lvar "a", z :: nil)
   
    val () = logats("before promoting [c] to [z]:")

    #define :: list_cons
    #define nil list_nil
    val-true = check_preorder (x, (Lvar"a" :: Lvar"z" :: Lvar"c" :: nil))

    val st = root(x)
    val st = down(st)
    val st = down(st)
    val st = promote(st)
    val x = host(st)

    val () = logats("after promoting [c] to [z]:")
    val-true = check_preorder (x, (Lvar"a" :: Lvar"c" :: nil))
   
    val () = tree_delete(x)
  }

  // treeinsert
  // insert at hole
  val () = logats("insert at hole")
  val () = {
    val x = hole()
     
    val st = root(x)
    val y = tree_atom(Lvar "A")
    val st = treeinsert (y, st)

    val x = host(st)
    val-true = check_preorder (x, list_sing(Lvar"A"))
     
    val () = tree_delete(x)
  }
  
  // insert at non-hole
  val () = logats("treeinsert test")
  val () = {
    val x = hole()
    
    val st = root(x)
    val st = treeinsert(tree_atom(Lvar"A"), st)
    val st = treeinsert(tree_fork(Ladd(), tree_atom(Lhole()) :: tree_atom(Lhole()) :: nil), st)
    val st = right(st)
    val st = treeinsert(tree_atom(Lvar"B"), st)
    val st = treeinsert(tree_fork(Ladd(), tree_atom(Lhole()) :: tree_atom(Lhole()) :: nil), st)
    val st = right(st)
    val st = treeinsert(tree_atom(Lvar"C"), st)

    val x = host(st)
    val n = N_of_tree (x)
    #define :: list_cons
    #define nil list_nil
    val-true =
      eq_N_N (n,
        N_node (Ladd(),
            N_leaf (Lvar "A") ::
            N_node (Ladd(), N_leaf (Lvar "B") :: N_leaf (Lvar "C") :: nil) :: nil
            )
      )
    
    val () = tree_delete(x)
  }

  // insert at non-hole
  val () = logats("insert at non-hole")
  val ()  = {
    val x = tree_fork(Lvar "plus", hole() :: hole() :: nil())
     
    val st = root(x)
    val y = tree_atom(Lvar "A")
    val st = treeinsert(y, st)
     
    val x = host(st)

    #define :: list_cons
    #define nil list_nil
    // FIXME: fails? why? also a LEAK!
    // seems like preorder is buggy!
    val-true = check_preorder (x, (Lvar"plus" :: Lvar"A" :: Lhole() :: nil()))
    
    val () = tree_delete(x)
  }

  // entry with precedence test 1
  val () = {
    val () = logats("simple entry test 1")
    
    val x = hole()
    val st = root(x)
    val st = entry (Lvar"a", st)
    val st = entry (Ladd(), st)
    
    val x = host(st)

    #define :: list_cons
    #define nil list_nil
    val-true = check_preorder (x, Ladd() :: Lvar"a" :: Lhole() :: nil)

    val () = tree_delete(x)
  }

  // entry with precedence test 2
  val () = {
    val () = logats("simple entry test 2")

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
    #define :: list_cons
    #define nil list_nil
    val n = N_of_tree x
    val-true =
      eq_N_N (n,
        N_node (
          Ladd(),
          N_node (
            Lmul(),
            N_leaf(Lvar"a") :: N_leaf(Lvar"b") :: nil
          ) ::
          N_node (
            Lmul(),
            N_leaf(Lvar"c") :: N_leaf(Lvar"d") :: nil
          ) :: nil)
      ) (* end of [val] *)
    val () = tree_delete(x)
  }
in
end

val () = hello ()

%{$
//
function
eq_JSarr_JSarr (f, xs, ys) {
  if (xs.length !== ys.length) {
    return false;
  }
  for (var i = 0; i < xs.length; i++) {
    if (!f(xs[i], ys[i])) {
      return false;
    }
  }
  return true;
}
function
eq_N_N(n1,n2) {
  return JSON.stringify(n1) === JSON.stringify(n2);
}
//
function
logats_ext(x) { console.log(x); }
//
my_dynload();
%}