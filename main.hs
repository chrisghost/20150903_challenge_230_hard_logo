import qualified Data.Text as T
import qualified Data.List.Split as S
import qualified Data.List as L

flatten :: [[a]] -> [a]
flatten = foldl (++) []

stripLines :: [String] -> [String]
stripLines l = map (\x -> T.unpack (T.strip x) ) (map T.pack l)

linesToWords :: [String] -> [String]
linesToWords l = flatten ( map (\s -> S.splitOn " " s) (stripLines l))

getWords :: [String] -> [String]
getWords l = filter (\x -> length x > 2) (linesToWords l)

extractWords :: [String] -> [String]
extractWords l = map (\w -> T.unpack (T.toLower (T.pack w) ) ) (getWords l ++ getWords (L.transpose l) )

realWords :: [String] -> [String] -> [String]
realWords l dict = L.sort ((filter (\x -> elem x dict) l) ++ (map reverse (filter (\x -> elem (reverse x) dict) (filter (\x -> notElem x dict) l))))

main = do
  file1 <- readFile "in"
  file2 <- readFile "in2"
  file3 <- readFile "in3"

  words <- readFile "words"
  let realDict = lines words

  putStrLn "Words 1 : "
  let w1 = extractWords (lines file1)
  mapM print (realWords w1 realDict)

  putStrLn "Words 2 : "
  let w2 = (extractWords (lines file2))
  mapM print (realWords w2 realDict)

  putStrLn "Words 3 : "
  let w3 = (extractWords (lines file3))
  mapM print (realWords w3 realDict)
