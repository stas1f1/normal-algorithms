{-
  На вход программе подаются название файла с схемой нормального алгорифма в формате
  a -> |b 
  b ->. | - завершающий
  и само слово, программа выдает "путь" к результату работы
-}

import System.Environment
import Data.List
import qualified Data.Text as T

data Algorithm = Algorithm {input :: String, output :: String, isFinal :: Bool }
    deriving Show

checkParseAlg :: String -> [String]
checkParseAlg s = let ss = words s in
    if length ss == 3 then ss else error ("wrong scheme format")

parsedToAlg :: [String] -> Algorithm
parsedToAlg ss = let input_ = head ss
                     input = if input_ == "eps" then "" else input_
                     action = head $ tail ss
                     output_ = last ss
                     output = if output_ == "eps" then "" else output_ in 
    if (action == "->") then Algorithm input output False
    else if (action == "->.") then Algorithm input output True else error("wrong scheme format")

strToAlg :: String -> Algorithm
strToAlg s = parsedToAlg $ checkParseAlg s

parseAlgs :: FilePath -> IO [Algorithm]
parseAlgs fp = do
    fc <- readFile fp
    pure $ map (\x -> strToAlg x) (lines fc)

isAppliable :: Algorithm -> String -> Bool
isAppliable (Algorithm input output isFinal) s = T.isInfixOf (T.pack input) (T.pack s)

isSchemeAppliable :: [Algorithm] -> String -> Bool
isSchemeAppliable ax s = any (\x -> isAppliable x s) ax

firstAppliable :: [Algorithm] -> String -> Algorithm 
firstAppliable ax s = case find (\x -> isAppliable x s) ax of
    Just alg -> alg
    Nothing -> error("code error: missed the check")

--Применение схемы. Первая строка в результате - полученное на этапе слово, вторая - "путь" к нему
applyScheme :: [Algorithm] -> String -> (String,String)
applyScheme [] s = (s,s)
applyScheme ax s = let appliable = isSchemeAppliable ax s in 
    if not appliable then (s,s)
    else let scheme = firstAppliable ax s 
             applied = applyAlg scheme s in 
        if isFinal scheme || (not $ isSchemeAppliable ax applied) then (applied, applied)
        else let next = applyScheme ax applied in  (fst next, unwords [applied,"=>",snd next]) 

applyAlg :: Algorithm -> String -> String
applyAlg (Algorithm input output isFinal) s = if input == "" then output ++ s
    else if T.isInfixOf (T.pack input) (T.pack s) then
    let ss = T.breakOn (T.pack input) (T.pack s) in concat $ [(T.unpack $ fst ss),(output),(drop (length input) $ T.unpack $ snd ss)] else s

execute :: [Algorithm] -> String -> IO()
execute algs s = do
    print $ snd $ applyScheme algs s

main = do
    args <- getArgs
    algs <- parseAlgs (head args)
    execute algs (head $ tail args)
