module Replace (
    ReplaceRule (ReplaceExact, ReplaceAllAfter, ReplaceExactAndAppend, ReplaceAllAfterUntil),
    applyReplaceRules,
    findSubStrIdx
) where

import Data.Maybe
import Data.List

data ReplaceRule = ReplaceExact String String | 
                   ReplaceExactAndAppend String String String | 
                   ReplaceAllAfter String String | 
                   ReplaceAllAfterUntil String String String

applyReplaceRules :: [ReplaceRule] -> String -> String
applyReplaceRules rules contents = 
    let contentLines = lines contents
    in unlines (map (\line -> foldl (\line2 rule -> applyReplaceRule rule line2) line rules) contentLines)

applyReplaceRule :: ReplaceRule -> String -> String
applyReplaceRule (ReplaceExact orig new) contents =
    applyRule contents

    where applyRule line = case findSubStrIdx line orig 0 of
                            Just i -> let splitContents = splitAt i line
                                      in applyRule $ fst splitContents ++ new ++ drop (length orig) (snd splitContents)
                            _      -> line

applyReplaceRule (ReplaceExactAndAppend orig new app) contents =
    let newContents = applyReplaceRule (ReplaceExact orig new) contents
    in if newContents == contents then contents else newContents ++ app

applyReplaceRule (ReplaceAllAfter orig new) contents =
    applyRule contents

    where applyRule line = case findSubStrIdx line orig 0 of
                            Just i -> let splitContents = splitAt i line
                                      in fst splitContents ++ new
                            _      -> line

applyReplaceRule (ReplaceAllAfterUntil orig new until) contents =
    applyRule contents

    where applyRule line = case findSubStrIdx line orig 0 of
                            Just i -> let splitContents = splitAt i line
                                      in case findSubStrIdx (snd splitContents) until 0 of
                                         Just i2 -> let splitContents2 = splitAt i2 (snd splitContents)
                                                    in applyRule (fst splitContents ++ new ++ snd splitContents2)
                                         _       -> line
                            _      -> line

findSubStrIdx :: String -> String -> Int -> Maybe Int
findSubStrIdx "" _ _ = Nothing
findSubStrIdx s target n
    | take (length target) s == target      = Just n
    | otherwise                             = findSubStrIdx (tail s) target (n + 1)