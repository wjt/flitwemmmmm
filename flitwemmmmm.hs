module Main where

import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random

compose :: [a -> a] -> (a -> a)
compose = foldl' (.) id

data Token = Letter Char
           | Extremity
    deriving (Show, Ord, Eq)

type BigramModel = Map Token (Seq Token)

updateOne :: Token -> Token -> (BigramModel -> BigramModel)
updateOne pre suc m =
    Map.insertWith (Seq.><) pre (Seq.singleton suc) m

updateWith :: String -> (BigramModel -> BigramModel)
updateWith title = compose updates
  where
    letters = map Letter title
    updates = zipWith updateOne (Extremity:letters) (letters ++ [Extremity])

buildModel :: [String] -> BigramModel
buildModel titles = compose (map updateWith titles) Map.empty

pickOne :: RandomGen g => BigramModel -> g -> Token -> (Token, g)
pickOne model gen pre = (Seq.index candidates i, gen')
  where
    -- This is safe because every character that comes out of the model has to
    -- have gone into the model, and even a character which only appears at the
    -- end of a track name will map to [Extremity].
    Just candidates = Map.lookup pre model

    n = Seq.length candidates
    (i, gen') = randomR (0, n-1) gen

inventName :: RandomGen g => BigramModel -> g -> (String, g)
inventName model = go Extremity
  where
    go pre gen = case pickOne model gen pre of
        (Extremity, gen') -> ("", gen')
        (Letter c, gen') -> let (cs, gen'') = go (Letter c) gen'
                            in  (c:cs, gen'')

main = do
    titles <- fmap lines getContents
    let model = buildModel titles
    gen <- getStdGen
    putStrLn . fst $ inventName model gen
