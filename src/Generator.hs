module Generator
  (
    Model

  , buildModel
  , inventName
  )
where

import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Random

compose :: [a -> a] -> (a -> a)
compose = foldl' (.) id

data Token = Letter Char
           | Extremity
    deriving (Show, Ord, Eq)

newtype Model = Model { unModel :: BigramModel }
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

buildModel :: [String] -> Model
buildModel titles = Model $ compose (map updateWith titles) Map.empty

pickOne :: RandomGen g => BigramModel -> Token -> Rand g Token
pickOne model pre = do
    -- This is safe because every character that comes out of the model has to
    -- have gone into the model, and even a character which only appears at the
    -- end of a track name will map to [Extremity].
    let Just candidates = Map.lookup pre model
    i <- getRandomR (0, Seq.length candidates - 1)
    return $ Seq.index candidates i

inventName :: RandomGen g => Model -> Rand g String
inventName (Model model) = go Extremity
  where
    go pre = do
        ret <- pickOne model pre
        case ret of
            Extremity -> return ""
            Letter c  -> do
                cs <- go ret
                return (c:cs)
