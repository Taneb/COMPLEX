module Interpreter where

import Control.Lens
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Linear.V2

import Debug.Trace

import Types

eval :: Registry -> Expression -> Integer
eval _ (Integer i) = i
eval r (Variable v) = case HM.lookup v r of
  Nothing -> error $ "NO SUCH VARIABLE: " ++ v
  Just i -> i
eval r (a :+ b) = eval r a + eval r b
eval r (a :- b) = eval r a - eval r b

exec :: Registry -> Instruction -> (Registry, Maybe String)
exec r (Print xs) = (r, Just $ intercalate " " . map (show . eval r) $ xs)
exec r (Set v x) = (HM.insert v (eval r x) r, Nothing)
exec r (If x i) = case eval r x of
  0 -> (r, Nothing)
  _ -> exec r i

move :: Registry -> LineNumber -> LineNumber
move r v = case V2 <$> HM.lookup "VECTORX" r <*> HM.lookup "VECTORY" r of
  Nothing -> error "OFFSET VECTOR LOST"
  Just w -> v + w

step :: Program -> Registry -> LineNumber -> (Registry, LineNumber, Maybe String)
step p r ln = case M.lookup ln p of
  Nothing -> (r, move r ln, Nothing)
  Just i -> case exec r i of
    (r', o) -> (r', move r' ln, o)

isInBoundingBox :: (V2 Integer, V2 Integer) -> V2 Integer -> Bool
isInBoundingBox (V2 lx ly, V2 hx hy) (V2 x y) =
  lx <= x && x <= hx &&
  ly <= y && y <= hy

run :: Program -> IO ()
run p = go (HM.fromList [("VECTORX", 1), ("VECTORY", 0)]) (V2 0 0)
  where
    boundingBox :: (V2 Integer, V2 Integer)
    boundingBox =
      let points = M.keys p
          lx = minimum $ map (view _x) points
          ly = minimum $ map (view _y) points
          hx = maximum $ map (view _x) points
          hy = maximum $ map (view _y) points
      in (V2 lx ly, V2 hx hy)
    go :: Registry -> LineNumber -> IO ()
    go r l | isInBoundingBox boundingBox l = case step p r l of
      (r', l', Nothing) -> go r' l'
      (r', l', Just o) -> putStrLn o >> go r' l'
           | otherwise = return ()
