module Eval where

import           AST
import           Prelude                 hiding ( Num )
import           Data.Map                hiding ( foldl
                                                , map
                                                )
import qualified Data.Map                      as Map

-- Let's start with the types

type Num = Integer

-- In order to treat "x := e" and "var := e" in the same
-- fashion, we expand the domain for variables with a formal
-- variable MetaVar
data Var = Var String | MetaVar
           deriving (Eq, Ord)

-- We need the types for the store/memory and the environment;
-- I elected not to introduce procedures explicitly, and rather
-- go with a minimalist approach of using "fix" to define the
-- recursive functions. Because of this, we will need a type
-- for setter.
-- Note: this approach is (in my estimation) far more difficult
-- than the one with explicit procedures with dynamic scope,
-- but I wanted to see how much more difficult it would be.

-- Due to the static scope requirement, we use locations in
-- the environment
type Loc = Integer
type Env = Map Var Loc

type Setter = Num -> Store -> Store

-- Memory contains both the current value, and the setter;
-- we also add some helper functions. newtype kills the
-- type synonym cycle error, we use store indirectly
-- anyways, and we can now define a custom Show
newtype Store = Store (Map Loc (Num, Setter))

instance Show Store where
  showsPrec prec (Store mu) =
    let mu_redux = Map.map fst mu in showsPrec prec mu_redux

valOf :: Loc -> Store -> Num
valOf ell (Store mu) = fst (mu ! ell)

setterOf :: Loc -> Store -> Setter
setterOf ell (Store mu) = snd (mu ! ell)

setVal :: Loc -> Num -> Store -> Store
setVal ell n mux@(Store mu) =
  Store (insert ell (n, setterOf ell mux) mu)

setSetter :: Loc -> Setter -> Store -> Store
setSetter ell set mux@(Store mu) =
  Store (insert ell (valOf ell mux, set) mu)

-- Now for the rest. Stmt doesn't change the environment, hence
-- the omission of Env from the return type
type Expr = (Env, Store) -> Num
type Decl = (Env, Store) -> (Env, Store)
type Stmt = (Env, Store) -> Store

-- Now, we can define the evaluation functions.

-- Num
evalNum :: ANum -> Num
evalNum ast = case ast of
  ANum n -> n

-- Var
evalVar :: AVar -> Var
evalVar ast = case ast of
  AVar x -> Var x

-- Expr
evalExpr :: AExpr -> Expr
evalExpr ast (rho, mu) = case ast of
  NumExpr n -> evalNum n
  VarExpr x -> let ell_x = rho ! (evalVar x) in valOf ell_x mu
  Op op e_1 e_2 ->
    let val_1  = evalExpr e_1 (rho, mu)
        val_2  = evalExpr e_2 (rho, mu)
        val_op = case op of
          Sum  -> (+)
          Prod -> (*)
          Diff -> (-)
    in  val_1 `val_op` val_2

-- Decl

-- evalDecl in itself is comparatively trivial
evalDecl :: ADecl -> Decl
evalDecl ast = case ast of
  SetterDecl x_1 x_2 s ->
    let x_1' = evalVar x_1
        x_2' = evalVar x_2
        s'   = evalStmt s
    in  setterDecl x_1' x_2' s'
  DeclSeq ds -> foldl (.) id (map evalDecl ds)

-- Now for the creation of setter Decl. We need to create
-- a recurrent version thereof, i.e. one that takes "itself"
-- amd yields itself. Let us restate the semantics:
-- 
-- "var x_1 set to x_2 by S"
-- -> introduces a new variable x_1 with the value of 0;
-- -> sets the setter of "var" to set x_1 (within S);
-- -> sets the setter of x_1 to S, where S n:
--    -> introduces a variable x_2 (within S);
--    -> sets the value of x_2 to n;
--    -> sets the setter of x_1 to "itself".
setterDecl :: Var -> Var -> Stmt -> Decl
setterDecl x_1 x_2 s (rho, mu) =
  let (rho', mu', ell_1) = newVar x_1 (rho, mu)
  in  let (rho'', mu'', ell_var) = newVar MetaVar (rho', mu')
      in  let mu3 = setSetter ell_var (proxy ell_1) mu''
          in  let recS = fix (recurrentS ell_1 x_2 s)
              in  let mu4 = setSetter ell_1 (freeze rho'' recS) mu3
                  in  (rho', mu4)

-- An intermediate type for the setter; this is because
-- recursive calls introduce new variables (or rather overshadow
-- x_2 with new locations).
type SetterRecur = Num -> Stmt

-- "Freeze" a SetterRecur into a Setter by fixing Env
freeze :: Env -> SetterRecur -> Setter
freeze rho_0 recur n mu = (recur n) (rho_0, mu)

-- Create a new variable, with "default" settings
newVar :: Var -> (Env, Store) -> (Env, Store, Loc)
newVar x (rho, mux@(Store mu)) =
  let ell_x = alloc mux
  in  let init_x = 0
          set_x  = proxy ell_x
      in  let rho' = insert x ell_x rho
              mu'  = Store (insert ell_x (init_x, set_x) mu)
          in  (rho', mu', ell_x)

-- Find a free location for a variable
alloc :: Store -> Loc
alloc (Store mu) = if Map.null mu then 0 else maximum (keys mu) + 1

-- Create a setter which "just" sets the memory
proxy :: Loc -> Setter
proxy = setVal

-- Recurrent version of S
recurrentS :: Loc -> Var -> Stmt -> SetterRecur -> SetterRecur
recurrentS ell_1 x_2 s recur n (rho, mu) =
  let (rho', mu', ell_2) = newVar x_2 (rho, mu)
  in  let mu'' = setVal ell_2 n mu'
      in  let mu3 = setSetter ell_1 (freeze rho' recur) mu''
          in  s (rho', mu3)

-- Fix operator
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Stmt

-- We shall, in this case, delegate all functions
-- so as to avoid the mess here
evalStmt :: AStmt -> Stmt
evalStmt ast = case ast of
  SetReal x e  -> assign (evalVar x) (evalExpr e)
  SetMeta e    -> assign MetaVar (evalExpr e)
  StmtSeq ss   -> stmtSeq (map evalStmt ss)
  If e s_t s_f -> ifStmt (evalExpr e) (evalStmt s_t) (evalStmt s_f)
  While e s    -> while (evalExpr e) (evalStmt s)
  Begin d s    -> begin (evalDecl d) (evalStmt s)

assign :: Var -> Expr -> Stmt
assign x e (rho, mu) =
  let val_e = e (rho, mu)
      ell_x = rho ! x
  in  (setterOf ell_x mu) val_e mu

stmtSeq :: [Stmt] -> Stmt
stmtSeq xs (rho, mu) = case xs of
  []       -> mu
  x : rest -> let mu' = x (rho, mu) in (stmtSeq rest) (rho, mu')

ifStmt :: Expr -> Stmt -> Stmt -> Stmt
ifStmt e s_t s_f (rho, mu) =
  let val_e = e (rho, mu)
  in  let s = if val_e == 0 then s_t else s_f in s (rho, mu)

-- Now, we get to "while e != 0 do S", which requires another use
-- of recurrent versions and fix
recurrentWhile :: Expr -> Stmt -> Stmt -> Stmt
recurrentWhile e s recur = ifStmt e (stmtSeq []) (stmtSeq [s, recur])

while :: Expr -> Stmt -> Stmt
while e s = fix (recurrentWhile e s)

begin :: Decl -> Stmt -> Stmt
begin d s = s . d

-- We shall also define some helper functions for running it
run :: AStmt -> Store
run s = (evalStmt s) (empty, Store empty)
