{-
yeah, move it to the browser

http://www.cs.ox.ac.uk/bernard.sufrin/edit.pdf

how to make it work in the browser?
- first idea:
  - input: wait for keyboard entry? say put an event handler on the whole page
  - output: just translate the tree after every edit into HTML
- use ATS + linear types and translate to JS

how would it work?
- https://github.com/daniel3735928559/guppy
- https://clearly.pl/
- maintain an HTML tree of divs or spans
- represent the current cursor/selection by some HTML element too
- BUT! how does it mesh with our current code??? doesn't blend in!

way forward: use abstract types instead of manipulating lists....
-}
data Tree = Fork !Label ![Tree]
  deriving (Eq, Ord, Show)
label :: Tree -> Label
label (Fork l _) = l
children :: Tree -> [Tree]
children (Fork _ ts) = ts
atomic :: Tree -> Bool
atomic = null . children
hole:: Tree
hole = Fork Hole []

type Path = [Layer]
type Layer = (Label{-label of focussed subtree-}, [Tree]{-children to the left-}, [Tree]{-children to the right-})
type SubTree = (Path, Tree)

embed :: Tree -> Layer -> Tree
embed t (l,left,right) = Fork l (reverse left++[t]++right)

selected :: SubTree -> Tree
selected (_, t) = t
path :: SubTree -> Path
path (p, _) = p

host :: SubTree -> Tree
host (p,t) = foldl embed t p

root :: Tree -> SubTree
root t = ([],t)

{-
host (root t) = t
selected(root t) = t
-}

backToTop :: SubTree -> SubTree
backToTop = until topmost up

left :: SubTree -> SubTree
left ((lab, last:l, r):ls, sel) = ((lab, l, sel:r):ls, last)
left st@((_, [], _):_, _) = st
left st@([], _) = st

leftmost :: SubTree -> Bool
leftmost ((_, [], _):_ ,_) = True
leftmost ([], _) = True
leftmost _ = False

firstChild :: SubTree -> SubTree -- moves to the left until leftmost
firstChild = until leftmost left

right :: SubTree -> SubTree
right st@((lab, l, first:r):ls, sel) =
  let res = ((lab, sel:l, r):ls, first) in res
 
right st@((_, _, []):_, _) = st
right st@([], _) = st

lastChild :: SubTree -> SubTree -- moves to the right until rightmost
lastChild = until rightmost right

rightmost :: SubTree -> Bool
rightmost ((_, _, []):_ ,_) = True
rightmost ([], _) = True
rightmost _ = False

{-
left (right st) = st, if not (rightmost st)
right (left st) = st, if not (leftmost st)
-}

up :: SubTree -> SubTree
up (layer:above,t) = (above, embed t layer)
up st@([],_) = st

topmost :: SubTree -> Bool
topmost ([], _) = True
topmost _ = False

down (p, Fork lab (t:ts))= ((lab, [], ts):p, t)
down st@(_, Fork _ []) = st
bottommost (_,Fork _ []) = True
bottommost _ = False

{-
down (up st) = st, if not (topmost st) && leftmost st
up (down st) = st, if not (bottommost st)
-}

atHole :: SubTree -> Bool
atHole (ls, (Fork Hole [])) = True
atHole _ = False

next :: SubTree -> SubTree
next st = if bottommost st then (rightup st) else (down st)

rightup :: SubTree -> SubTree
rightup = right . until (\st -> topmost st || not (rightmost st)) up

preorder :: Tree -> [Tree]
preorder = map selected . takeOneWhile (not . topmost) . iterate next . root

takeOneWhile :: (SubTree -> Bool) -> [SubTree] -> [SubTree]
takeOneWhile p xs = takeWhile p xs

nextSuchThat :: (SubTree -> Bool) -> SubTree -> SubTree
nextSuchThat p st =
    if p st' then st' else st
    where st' = until (\ st -> topmost st || p st) next (next st)

{-
The search stops if a topmost node is reached, but note that

nextSuchThat p st = st

only if st has no proper next successor that satisfies p.
-}
replace :: Tree -> SubTree -> SubTree
replace s (p, t) = (p, s)

{-
replace (selected st) (replace s st) = st
-}

insert :: Label -> SubTree -> SubTree
insert lab st =
  let r = template lab in
  let st' = treeinsert r st in
  st'

treeinsert :: Tree -> SubTree -> SubTree
treeinsert t st = if atHole st then(replace t st)
  else ((replace (selected st) . down . replace t)) st

{-
selection(treeinsert t st) = selection st, if not (atHole st)
-}

kill :: SubTree -> SubTree
kill = replace hole

promote :: SubTree -> SubTree
promote st = (replace (selected st) . up) st

{-
kill (treeinsert t st) = st, if atHole st
promote (treeinsert t st) = st, if not (atHole st) && not(atomic t)
id (treeinsert t st) = st, if not (atHole st) && atomic t
replace (selected st) (kill st) = st
-}

situation :: SubTree -> Path
situation (c:cs, t) = [c]
situation ([], t) = []
graft :: Path -> SubTree -> SubTree
graft cs (p, t) = (cs++p, t)
{-
graft (situation st) (promote st) = st
-}

enter :: Label -> SubTree -> SubTree
enter l = nextSuchThat atHole . insert l

entry :: Label -> SubTree -> SubTree
entry l = enter l . reduce l

reduce :: Label -> SubTree -> SubTree
reduce l = until (irreducible l) up

irreducible :: Label -> SubTree -> Bool
irreducible l st = atHole st || topmost st || (not . rightmost) st || (not . producable l . label . selected . up) st

producable :: Label -> Label -> Bool
producable op2 op1 = (op1==op2 && assl op1) || prec op1 > prec op2

type SubTree' = ([Path],Tree)

unflatten :: SubTree -> SubTree'
unflatten (ps, t) = (ps:[], t)

flatten :: SubTree' -> SubTree
flatten (ps,t) = (concat ps,t)

lift :: (SubTree -> SubTree) -> (SubTree' -> SubTree')
lift f (p:ps,t) = (q:ps,s) where (q,s) = f (p,t)

entry' = lift . entry
left' = lift left
firstChild' = lift firstChild
right' = lift right
lastChild' = lift lastChild
up' = lift up
down' = lift down
backToTop' = lift backToTop

insert' = lift . insert
replace' = lift . replace

open :: SubTree' -> SubTree'
open (p,t) = ([]:p,t)

close :: SubTree' -> SubTree'
close (p:ps,t) = right' (ps, s) where s = host (p, t)

{- ------------------------- -}
-- anything to do with the target language

data Label =
  Hole
  | If
  | Add
  | Mul
  | Var !String
  | Const !Int
  | Lam
  | Annot -- type annotation
  | App -- function application
  deriving (Eq, Ord, Show)

assl :: Label -> Bool {- associates to the left -}
assl Add = True
assl Mul = True
assl App = True
assl _ = False

prec :: Label -> Int {- precedence -}
prec App = 1500
prec If = 1000
prec Add = 100
prec Mul = 120
prec Annot = 50
prec Lam = -1
prec _ = 0

template :: Label -> Tree
template App = Fork App [hole,hole]
template If = Fork If [hole, hole, hole]
template Lam = Fork Lam [hole, hole]
template Add = Fork Add [hole, hole]
template Mul = Fork Mul [hole, hole]
template Annot = Fork Annot [hole, hole]
template l = Fork l []

{- ------------------------- -}
-- simple driver for testing

data NavType = NTleft | NTright | NTup | NTdown | NTfirst | NTlast | NTtop
  deriving (Eq, Ord, Show)
data Command =
    Entry !Tree -- begin with this tree
  | LParen -- open paren
  | RParen -- close paren
  | Move !NavType -- navigate the tree
  | Replace !Tree -- replace focussed subtree with the new tree
  | Insert !Label -- insert a new label
  | Enter !Label -- enter a new label, taking precedence into account
  deriving (Eq, Ord, Show)
data ModelState =
    MSnothing
  | MSsubtree !SubTree
  | MSparenthesized !SubTree'
  deriving (Eq, Ord, Show)
type Model =
  -- either SubTree, or SubTree' or Nothing
  (ModelState, String)

present :: Command -> Model -> Model
present (Entry t) d = (MSparenthesized (unflatten (root t)),  "")
present (Move nt) (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot move without a tree")
    MSsubtree x -> (MSsubtree ((case nt of
                        NTleft -> left
                        NTright -> right
                        NTup -> up
                        NTdown -> down
                        NTfirst -> firstChild
                        NTlast -> lastChild
                        NTtop -> backToTop) x), "")
    MSparenthesized x ->
        (MSparenthesized ((case nt of
                            NTleft -> left'
                            NTright -> right'
                            NTup -> up'
                            NTdown -> down'
                            NTfirst -> firstChild'
                            NTlast -> lastChild'
                            NTtop -> backToTop') x), "")
present (Replace t) (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot replace without a tree")
    MSsubtree x -> (MSsubtree (replace t x), "")
    MSparenthesized x -> (MSparenthesized (replace' t x), "")
present (Insert lab) (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot insert without a tree")
    MSsubtree x -> (MSsubtree (insert lab x), "")
    MSparenthesized x -> (MSparenthesized (insert' lab x), "")
present (Enter lab) (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot enter without a tree")
    MSsubtree x -> (MSsubtree (entry lab x), "")
    MSparenthesized x -> (MSparenthesized (entry' lab x), "")
present LParen (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot insert left paren without a tree")
    MSsubtree (x,t) -> (MSparenthesized (x:[], t), "") -- begin a parenthesized group
    MSparenthesized t -> (MSparenthesized (open t), "")
present RParen (tree, errorMsg) =
  case tree of
    MSnothing -> (tree, "Cannot insert right paren without a tree")
    MSsubtree t -> (MSsubtree t, "") -- leave it the same!
    MSparenthesized t -> (MSparenthesized (close t), "")
present c (d, _) = (d, "Unable to perform command: " ++ show c)

scriptPresent :: Model -> [Command] -> IO Model
scriptPresent mdl [] = return mdl
scriptPresent mdl@(dat, errmsg) (x:xs) = do
  print ("applying cmd: " ++ show x)
  let mdl' = present x mdl
  print ("result: " ++ show mdl')
  scriptPresent mdl' xs
scriptPresentResult :: Model -> [Command] -> IO Tree
scriptPresentResult mdl xs = do
  (d, e) <- scriptPresent mdl xs
  case d of
    MSnothing -> fail "should have been some data"
    MSsubtree x -> return (host x)
    MSparenthesized x -> return (host . flatten $ x)

checkExpect :: (Eq a, Show a) => a -> a -> IO ()
checkExpect x y =
  if x == y then
    putStrLn $ "OK: " ++ show x
  else do
    putStrLn "Error! Mismatched terms"
    putStrLn $ "Expected: " ++ show x
    putStrLn $ "Actual: " ++ show y
    fail "should have been equal"

main :: IO ()
main = do
  print "test A"
  e1 <- scriptPresentResult
          (MSnothing, "")
          [Entry (template Add), Move NTdown, Insert (Const 1), Move NTright, Insert (Const 2), Move NTup]
  checkExpect (Fork Add [Fork (Const 1) [], Fork (Const 2) []]) e1
 
  print "test B"
  e2 <- scriptPresentResult
        (MSnothing, "")
        [
          Entry hole,
          Insert (Var "A"),
          Insert Add,
          Move NTright,
          Insert (Var "B"),
          Insert Add,
          Move NTright,
          Insert (Var "C")] -- result should be A + (B + C)
  checkExpect (Fork Add [Fork (Var "A") [], Fork Add [Fork (Var "B") [], Fork (Var "C") []]]) e2

  print "test C"
  -- a*b+c*d
  e3 <- scriptPresentResult
    (MSnothing, "")
    [Entry hole, Enter (Var "A"), Enter Mul, Enter (Var "B"), Enter Add, Enter (Var "C"), Enter Mul, Enter (Var "D")]
  checkExpect
    (Fork Add [
      Fork Mul [Fork (Var "A") [], Fork (Var "B") []],
      Fork Mul [Fork (Var "C") [], Fork (Var "D") []]])
    e3
   
  print "test D"
  -- (a+b)*(c+d)
  e4 <- scriptPresentResult
    (MSnothing, "")
    [Entry hole,
      LParen, Enter (Var "A"), Enter Add, Enter (Var "B"), RParen,
      Enter Mul,
      LParen, Enter (Var "C"), Enter Add, Enter (Var "D"), RParen]
  checkExpect
    (Fork Mul [
      Fork Add [Fork (Var "A") [], Fork (Var "B") []],
      Fork Add [Fork (Var "C") [], Fork (Var "D") []]])
    e4

{-
  print "test D0'"
  let r0 = root (Fork If [hole,Fork (Var "A") [],hole])
  print $ "r0 = " ++ show r0
  let r1 = next r0 -- down r0
  print $ "r1 = " ++ show r1
  --let r2 = right r1
  --print $ "r2 = " ++ show r2
  -}
 
  -- NOTE: still a difference between enter & insert...
  -- the below is not possible to do with "Enter" alone
  print "test D0"
  e4_0 <- scriptPresentResult (MSnothing, "") [Entry hole, Enter If, Enter (Var "x")]
  checkExpect (Fork If [Fork (Var "x") [],hole,hole]) e4_0

  print "test D1"
  -- "if a then b else c"
  e4_1 <- scriptPresentResult
    (MSnothing, "")
    [Entry hole, Enter If, Enter (Var "A"), Enter (Var "B"), Enter (Var "C")]
  checkExpect
    (Fork If [Fork (Var "A") [], Fork (Var "B") [], Fork (Var "C") []])
    e4_1

  print "test E"
  -- (if a then b else c) + d
  e5 <- scriptPresentResult
    (MSnothing, "")
    [Entry hole, LParen, Enter If, Enter (Var "A"), Enter (Var "B"), Enter (Var "C"), RParen, Enter Add, Enter (Var "D")]
  checkExpect
    (Fork Add [
      Fork If [
        Fork (Var "A") [],
        Fork (Var "B") [],
        Fork (Var "C") []
      ],
      Fork (Var "D") []])
    e5

  -- \ab+c*d
  print "test F"
  e6 <- scriptPresentResult
      (MSnothing, "")
      [Entry hole, Enter Lam, Enter (Var "A"), Enter (Var "B"), Enter Add, Enter (Var "C"), Enter Mul, Enter (Var "D")]
  checkExpect
    (Fork Lam [
      Fork (Var "A") [],
      Fork Add [
        Fork (Var "B") [],
        Fork Mul [
          Fork (Var "C") [],
          Fork (Var "D") []
        ]
        ]
      ])
    e6

  -- a + b * d : t
  print "test G"
  e7 <- scriptPresentResult
      (MSnothing, "")
      [Entry hole, Enter (Var "A"), Enter Add, Enter (Var "B"), Enter Mul, Enter (Var "D"), Enter Annot, Enter (Var "T")]
  checkExpect
    (Fork Annot [
      Fork Add [
        Fork (Var "A") [],
        Fork Mul [
          Fork (Var "B") [],
          Fork (Var "D") []
          ]
        ],
      Fork (Var "T") []
      ])
    e7
 
  print "test fun-app"
  e7 <- scriptPresentResult
    (MSnothing, "")
    [Entry hole, Enter (Var "F"), Enter App, Enter (Var "X"), Enter App, Enter (Var "Y")]
  checkExpect
    (Fork App [Fork App [Fork (Var "F") [], Fork (Var "X") []], Fork (Var "Y") []])
    e7
   

{-
real languages?
- freeform comment nodes
- begin end blocks (or the like, e.g. { } blocks)
- different kind of brackets

some languages also have variadic constructs
- e.g. f (x, y)
- lists, arrays, etc. (literals), e.g. [a,b,c,d]
  - this is same as with fun-app, actually
- how do we handle fun-app ???
 (a b) --> (a Apply b)
 fun-app associates to the left:
 f x y z ==> ((f x) y) z
some languages also have different constructs
- let|local-in-end
- E where E1
- fun|val a = b  (declaration list)

ATS to JS?

undo?

what's a fun real language? say regexes? or relational algebra

One of the salient features of the model is the representation of a selection as a sequence of
context layers paired with the selected subtree. This representation makes it easy to reason about
selections – and as an added bonus its Haskell implementation is quite efficient. Moreover it can
straightforwardly be generalised to support a style of selection in which a number of adjacent
siblings are simultaneously selected. This style of selection is necessary when editing structures
which use variadic nodes to represent trees formed of semantically associative operators – such
as sequential composition (of commands in a programming language), and juxtaposition (of
words, paragraphs, etc. in a natural language).
- what is this style of selection?
- what if you had multiple trees under selection/focus AT ONCE? e.g. a list of trees
- you could also make a separate MODE for that. so if user enters BEGIN, say
  - the editor starts the mode where it will

A further layer of design which we hinted at earlier introduces a scratchpad and cut buffer,
and supports more radical structural changes and better undoability properties. The details are
straighforward and only space considerations led us to exclude them.

Finally, we have ignored user-interface issues related to input of commands, to layout of the
visible representation of the tree, and to navigation and selection using mouse gestures. We
intend to pursue all these in a subsequent paper in the same constructive mathematical style.

-}