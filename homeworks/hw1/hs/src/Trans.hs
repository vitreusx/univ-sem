{-# LANGUAGE TupleSections, TemplateHaskell #-}

module Trans where

import           Data.Map.Strict               as Map
import           Data.Set                      as Set
import           Control.Monad
import           Data.Maybe
import           Control.Lens
import           Data.Functor

type Var = String
type VarMem = Map Var Integer

type Trans = String
type TransMem = Set Trans

data State = State
  { _varMem  :: VarMem
  , _trFlag  :: Maybe Trans
  , _commits :: Map Trans VarMem
  , _trStack :: [Trans]
  }
  deriving Show

initState = State { _varMem  = Map.empty
                  , _trFlag  = Nothing
                  , _commits = Map.empty
                  , _trStack = []
                  }

$(makeLenses ''State)

newtype TransST a = TransST (State -> (Maybe a, State))

run (TransST xsf) = xsf
eval xs = fromJust $ fst $ run xs initState

instance Functor TransST where
  fmap f xs =
    TransST (\s -> let (x, s') = run xs s in (f <$> x, s'))

instance Applicative TransST where
  pure = return
  fs <*> xs = do
    f <- fs
    fmap f xs

instance Monad TransST where
  return x = TransST (\s -> (Just x, s))

  xs >>= f = TransST
    (\s ->
      let (x, s') = run xs s
      in  case x of
            Just x' -> run (f x') s'
            Nothing -> (Nothing, s')
    )

get = TransST (\s -> (Just s, s))
gets f = liftM f get
put s = TransST (\s' -> (Just (), s))
modify f = get >>= (put . f)

ensureVar name = do
  vm <- gets $ view varMem
  when (not (Map.member name vm)) (setVar name 0)

commitFor tr = do
  s <- get
  let alter = Map.alter (const $ Just $ view varMem s) tr
  modify $ over commits alter

commit = do
  comKeys <- gets (keys . view commits)
  forM_ comKeys commitFor

guard = TransST
  (\s ->
    let v = if isJust (view trFlag s) then Nothing else Just ()
    in  (v, s)
  )

fail tr = do
  ts <- gets $ view trStack
  when (elem tr ts) $ do
    modify (set trFlag $ Just tr)
    Trans.guard

ignoreGuard xs =
  TransST (\s -> let (v, s') = run xs s in (Just (), s'))

try tr xs = do
  commitFor tr
  modify $ over trStack $ (++) [tr]
  ignoreGuard xs
  modify $ over trStack tail
  s <- get
  if Just tr == view trFlag s
    then do
      let resetMem  = set varMem $ view commits s ! tr
      let resetFlag = set trFlag Nothing
      modify (resetFlag . resetMem)
    else Trans.guard

val v = do
  s <- get
  return $ view varMem s ! v

setVar x v = modify $ over varMem $ alter (const $ Just v) x

dump xs = xs >> get
