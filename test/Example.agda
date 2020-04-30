
module Example where

open import Prelude
open import System.Console.Readline

names : List (List String)
names = map words
      $ "Ulf Norell"
      ∷ "Ulf Lundell"
      ∷ "Luke Skywalker"
      ∷ "Luke Cage"
      ∷ "Luna Lovegood"
      ∷ []

completion : String → List String
completion s = nub $ do
  let ws = words s
  just (next ∷ _) ← map (dropPrefix ws) names
    where _ → []
  pure next

main : IO ⊤
main = do
  setCompletion (pure ∘ completion)
  just s₀ ← readline "What's your name? "
    where nothing → putStrLn "\nOk, nevermind."
  let s = trim s₀
  addHistory s
  putStrLn $ "Hello " & s & "!"
