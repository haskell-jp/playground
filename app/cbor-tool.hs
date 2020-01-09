{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

-- cabal new-run cbor-tool resources/cbor-tool/sample.cbor

import Codec.CBOR.Read
import Codec.CBOR.Term
import Data.List
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens hiding (List, Context(..))
import Control.Monad
import System.IO
import qualified Data.Text as T
import System.Directory
import Data.Char
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

dispKeyValue :: Term -> Term -> Doc AnsiStyle
dispKeyValue k v = mconcat
    [ annotate bold $ disp k
    , ": "
    , disp v
    ]

_testData :: Term
_testData = TMap
  [(TString "Int", TInt 42)
  ,(TString "Integer", TInteger 36893488147419103232)
  ,(TString "Bytes", TBytes "\0\1\2\3\4")
  ,(TString "String", TString "ハスケル")
  ,(TString "List", TList [TInt 0, TString "one"])
  ,(TString "Map", TMap [(TString "foo", TString "bar"), (TInt 1, TInt 0)])
  ,(TString "Tagged", TTagged 1 $ TString "2020-01-01T00:00Z")
  ,(TString "Float", TDouble pi)
  ,(TString "Double", TDouble pi)
  ,(TString "Null", TNull)
  ]

disp :: Term -> Doc AnsiStyle
disp (TInt i) = viaShow i
disp (TInteger i) = viaShow i
disp (TBytes s) = viaShow s
disp (TBytesI s) = viaShow s
disp (TString s) = annotate (color Blue) $ dquotes $ pretty s
disp (TStringI s) = annotate (color Blue) $ dquotes $ pretty s
disp (TList xs) = list $ map disp xs
disp (TListI xs) = list $ map disp xs
disp (TMap xs) = encloseSep lbrace rbrace comma $ map (uncurry dispKeyValue) xs
disp (TMapI xs) = encloseSep lbrace rbrace comma $ map (uncurry dispKeyValue) xs
disp (TTagged i x) = viaShow i <> parens (disp x)
disp (TBool x) = viaShow x
disp TNull = "()"
disp (TSimple x) = viaShow x
disp (THalf x) = viaShow x
disp (TFloat x) = viaShow x
disp (TDouble x) = viaShow x

showPattern :: Term -> String
showPattern (TString s) = T.unpack s
showPattern t = "'" ++ show (disp t) ++ "'"

completion :: Term -> Int -> [String] -> [String]
completion (TMap xs) 0 (s:_) = [d | (k, _) <- xs, let d = showPattern k, isPrefixOf s d]
completion (TMap xs) 0 _ = map (showPattern . fst) xs
completion (TMap xs) n ("_":ss) = [r | (_, v) <- xs, r <- completion v (n - 1) ss]
completion (TMap xs) n (s:ss) = [r | (k, v) <- xs, let d = showPattern k, s == d, r <- completion v (n - 1) ss]
completion (TList xs) n (s:ss) = xs ^.. ix (read s) . folding (\t -> completion t (n - 1) ss)
completion _ _ _ = []

access :: Term -> [String] -> [Doc AnsiStyle]
access (TMap xs) ("_":ss) = [r | (_, v) <- xs, r <- access v ss]
access (TMap xs) (s:ss) = [r | (k, v) <- xs, quoted s == showPattern k, r <- access v ss]
access (TList xs) (s:ss) = xs ^.. ix (read s) . folding (\t -> access t ss)
access t _ = [disp t]

quoted :: String -> String
quoted str
    | all (\c -> isAlphaNum c || c `elem` ("-./" :: String)) str = str
    | otherwise = "'" ++ concatMap (\c -> if c == '\'' then "\\'" else [c]) str ++ "'"

runCompletion :: Int -> [String] -> IO ()
runCompletion n ("--bash-completion-word" : "cbor-tool" : xs) = runCompletion (n - 1) xs
runCompletion 0 ("--bash-completion-word" : path : _) = getDirectoryContents "." >>= mapM_ putStrLn . filter (isPrefixOf path)
runCompletion 0 _ = getDirectoryContents "." >>= mapM_ putStrLn
runCompletion n ("--bash-completion-word" : path : xs) = do
    term <- BL.readFile path >>= deserialise
    mapM_ putStrLn $ completion term (n - 1) $ filter (/="--bash-completion-word") xs
runCompletion _ _ = error "runCompletion: unimplemented"

deserialise :: BL.ByteString -> IO Term
deserialise = either (fail . show) (pure . snd) . deserialiseFromBytes decodeTerm

parseArgs :: [String] -> IO ()
parseArgs ("--bash-completion-index" : n : xs) = runCompletion (read n) xs
parseArgs ("--multiple" : xs) = forever $ do
    n <- readLn
    term <- BL.hGet stdin n >>= deserialise
    mapM_ (putDoc . (<>hardline)) $ access term xs
parseArgs (path : xs) = do
    term <- BL.readFile path >>= deserialise
    mapM_ (putDoc . (<>hardline)) $ access term xs
parseArgs [] = fail "cbor-tool PATH [query]"

main :: IO ()
main = getArgs >>= parseArgs
