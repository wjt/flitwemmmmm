module Generator
  (
    Model

  , readModel
  , buildModel
  , inventName

  -- Really belongs in a Util.hs but...
  , randomElem
  )
where

import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Random
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

compose :: [a -> a] -> (a -> a)
compose = foldl' (.) id

data Token = Letter Char
           | Extremity
    deriving (Show, Ord, Eq)

isLetter, isExtremity :: Token -> Bool
isLetter (Letter _) = True
isLetter _          = False
isExtremity = not . isLetter

newtype Model = Model BigramModel
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

buildModel :: [Text] -> Model
buildModel titles = Model $ compose (map (updateWith . Text.unpack) titles) Map.empty

readModel :: FilePath -> IO Model
readModel f = do
    contents <- TextIO.readFile f
    return $ buildModel (Text.lines contents)

randomElem :: RandomGen g
           => Seq a
           -> Rand g a
randomElem xs = do
    i <- getRandomR (0, Seq.length xs - 1)
    return $ Seq.index xs i

pickOne :: RandomGen g
        => BigramModel
        -> Token
        -> Bool         -- ^ exclude Extremity from the candidates if possible
        -> Rand g Token
pickOne model pre excludeExtremity = do
    -- This is safe because every character that comes out of the model has to
    -- have gone into the model, and even a character which only appears at the
    -- end of a track name will map to [Extremity].
    let Just candidates = Map.lookup pre model
        candidateLetters = Seq.filter isLetter candidates
        -- If requested, try to avoid ending the track name here.
        candidates' = if excludeExtremity && not (Seq.null candidateLetters)
                          then candidateLetters
                          else candidates

    randomElem candidates'

inventName :: RandomGen g => Model -> Rand g Text
inventName (Model model) = go Extremity 0
  where
    go pre i = do
        ret <- pickOne model pre (i < 2)
        case ret of
            Extremity -> return Text.empty
            Letter c  -> Text.cons c `fmap` go ret (i + 1)
