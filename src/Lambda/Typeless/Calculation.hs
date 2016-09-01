module Lambda.Typeless.Calculation where

import Control.Lens
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Cause
  = Result
    -- ^ Normal calculation finished
  | Error String
    -- ^ Error because term malformed
  deriving Show

data Stop = Stop
  { _stopCause :: Cause
  , _stopInner :: Term
    -- ^ Inner term calculation was stopped in
  } deriving Show

makeLenses ''Stop

type CalcStep = Either Stop

data Tick = Step Term | Rename Term | Finish Stop
          deriving Show

tickEither :: Tick -> Either Stop Term
tickEither = \case
  Step t   -> Right t
  Rename t -> Right t
  Finish s -> Left s

-- | Assumes all binded variables are uniqe
fullReduce :: Term -> CalcStep Term
fullReduce t = case t of
  TVar _ ->
    Left $ Stop Result t
  TAbs v term -> TAbs v <$> fullReduce term
  TApp abst arg -> case abst of
    TAbs v term -> pure $ replaceVar v arg term
    _           -> case fullReduce abst of
      Right nuAbs -> Right $ TApp nuAbs arg
      Left stop -> case stop ^. stopCause of
        Result -> TApp abst <$> fullReduce arg
        _      -> Left stop

replaceVar
  :: Var
     -- ^ var to replace
  -> Term
     -- ^ replace with
  -> Term
     -- ^ replace where
  -> Term
replaceVar v trepl twhere = case twhere of
  TVar lv
    | lv == v   -> trepl
    | otherwise -> twhere
  TAbs lv term ->
    TAbs lv $ replaceVar v trepl term
  TApp abst arg -> TApp (replaceVar v trepl abst) (replaceVar v trepl arg)

-- | Returns list of free variables. Closed term have no free variables
freeVars :: Term -> Set Var
freeVars = go S.empty
  where
    go scope t = case t of
      TVar v
        | S.notMember v scope -> S.singleton v
        | otherwise           -> S.empty
      TAbs v term   -> go (S.insert v scope) term
      TApp abst arg -> S.union (go scope abst) (go scope arg)

renameVars :: Term -> Term
renameVars term' =
  let startVar = maybe 0 (succ . fst) $ S.maxView $ freeVars term'
  in evalState (go M.empty term') startVar
  where
    go :: Map Var Var -> Term -> GTerm
    go repVar t = case t of
      TVar v -> case M.lookup v repVar of
        Nothing     -> pure t        -- seems variable is free
        Just newVar -> pure $ TVar newVar
      TAbs v term -> do
        newVar <- nextVar
        TAbs newVar <$> go (M.insert v newVar repVar) term
      TApp abst arg -> TApp <$> go repVar abst <*> go repVar arg


-- | Generates potentially infinite list of calculation steps. Assumes first
-- term have uniq binded variables.
calculation :: (Term -> CalcStep Term) -> Term -> [Tick]
calculation step term =
  let
    renTerm = renameVars term
    rest = case step renTerm of
      Left stop -> case stop ^. stopCause of
        Result -> []            -- previous term is result
        _ -> [Finish stop]
      Right nextTerm -> calculation step nextTerm
  in Step term : Rename renTerm : rest

justCalc :: Term -> CalcStep (Int, Term)
justCalc t =
  let (f, l) = L.splitAt 1000 $ calculation fullReduce t
  in case l of
    [] -> case tickEither $ last f of
      Right a   -> Right (length f, a)
      Left stop -> Left stop
    (x:_) -> either Left (Left . Stop (Error "Too much steps")) $ tickEither x
