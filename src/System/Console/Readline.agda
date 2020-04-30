
module System.Console.Readline where

open import Prelude
open import Prelude.Variables
open import Foreign.Haskell.Types

{-# FOREIGN GHC import Data.Text (pack, unpack) #-}
{-# FOREIGN GHC import System.Console.Editline.Readline #-}

-- Minimal interface ---

postulate
  readline              : String → IO (Maybe String)
  addHistory            : String → IO ⊤
  setCompletionFunction : Maybe (String → IO (List String)) → IO ⊤
  getLineBuffer         : IO String

private
  postulate
    hsSetAttemptedCompletionFunction :
      Maybe (String → Nat → Nat → IO (Maybe (HSTuple String (List String)))) → IO ⊤

setAttemptedCompletionFunction :
      Maybe (String → Nat → Nat → IO (Maybe (String × List String))) → IO ⊤
setAttemptedCompletionFunction nothing  = hsSetAttemptedCompletionFunction nothing
setAttemptedCompletionFunction (just f) =
  hsSetAttemptedCompletionFunction (just λ s a b → (fmap ∘ fmap) (λ (s , ss) → s , ss)
                                                   (f s a b))

{-# COMPILE GHC readline      = (fmap . fmap) pack . readline . unpack #-}
{-# COMPILE GHC addHistory    = addHistory . unpack #-}
{-# COMPILE GHC getLineBuffer = fmap pack $ getLineBuffer #-}
{-# COMPILE GHC setCompletionFunction =
      setCompletionEntryFunction
      . fmap (\ f -> ((fmap . map) unpack . f . pack)) #-}
{-# COMPILE GHC hsSetAttemptedCompletionFunction =
      let marshal f s a b = do
            { mres <- f (pack s) (fromIntegral a) (fromIntegral b)
            ; return $ fmap (\ (s , ss) -> (unpack s , map unpack ss)) mres }
      in setAttemptedCompletionFunction . fmap marshal #-}

-- Nicer functions --

clearCompletion : IO ⊤
clearCompletion = setAttemptedCompletionFunction nothing

private
  strsCommonPrefix : List String → String
  strsCommonPrefix []       = ""
  strsCommonPrefix (s ∷ ss) = foldl strCommonPrefix s ss

-- | Completion function gets line before the incomplete word and should return
--   possible next words.
setCompletion : ((before : String) → IO (List String)) → IO ⊤
setCompletion f = setAttemptedCompletionFunction $ just λ incomplete pos _ → do
  before ← strTake pos <$> getLineBuffer
  caseM filter (strIsPrefixOf? incomplete) <$> f before of λ where
    []      → pure nothing
    [ cmd ] → pure (just (cmd , []))
    cmds    → pure (just (strsCommonPrefix cmds , cmds))
