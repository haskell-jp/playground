{-# LANGUAGE Rank2Types, DeriveFunctor #-}
{-
`harvester [expr]`

reads comma-separated lists of numbers from the standard input, and prints the result.

## Examples

```
harvester len 'sum $1'
```

Returns the length and the sum of the first column.

```
harvester 'dist (bucket 0.1 0.0 $1) (sum ($2 * $3) / sum $2)'
```

Calculates an average of the third column weighted by the second column for each interval.
-}

import Control.Applicative
import Control.Comonad
import qualified Control.Foldl as F
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Text.Trifecta
import Text.Parser.Expression
import Text.Read (readMaybe)
import System.Environment
import System.IO (stderr)
import Data.Text.Prettyprint.Doc (hardline)
import Data.Text.Prettyprint.Doc.Render.Terminal (hPutDoc)

data Bucket = MkBucket !Int !Double

instance Eq Bucket where
  MkBucket i _ == MkBucket j _ = i == j

instance Ord Bucket where
  MkBucket i _ <= MkBucket j _ = i <= j

data Val = Dbl !Double
  | Bucket !Bucket
  | List [Val]
  | Map !(Map.Map Val Val)
  deriving (Eq, Ord)

data Prim = PDbl !Double
  | PBucket !Double !Double !Double !(Expr Prim)
  | PList [Expr Prim]
  | PCol !Int

data Aggregation = Sum (Expr Prim)
  | Distrib (Expr Prim) Aggregation
  | Len
  | Expr (Expr Aggregation)

data Expr a = Val a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  | Abs (Expr a)

parseAggregation :: Parser Aggregation
parseAggregation = choice
  [ Expr <$> parens (parseExpr parseAggregation)
  , Sum <$> (symbol "sum" *> parseTerm prim)
  , symbol "dist" *> (Distrib <$> parseTerm prim <*> parseAggregation)
  , Len <$ symbol "len"
  ]

prim :: Parser Prim
prim = choice
  [ fmap PCol $ symbol "$" *> fmap fromIntegral natural
  , symbol "bucket" *> (PBucket <$> num <*> num <*> num <*> parseTerm prim)
  , brackets $ PList <$> commaSep (parseExpr prim)
  , PDbl <$> num
  ]

num :: Parser Double
num = either fromIntegral id <$> integerOrDouble
   <|> parens (either fromIntegral id <$> integerOrDouble)

parseTerm :: Parser a -> Parser (Expr a)
parseTerm m = choice
  [ parens (parseExpr m)
  , Val <$> m
  ]

parseExpr :: Parser a -> Parser (Expr a)
parseExpr m = buildExpressionParser
  [ [Prefix (Abs <$ symbol "abs")]
  , [Infix (Pow <$ symbol "^") AssocRight]
  , [Infix (Mul <$ symbol "*") AssocLeft, Infix (Div <$ symbol "/") AssocLeft]
  , [Infix (Add <$ symbol "+") AssocLeft, Infix (Sub <$ symbol "-") AssocLeft]
  ]
  (parseTerm m)

toFold :: Aggregation -> F.Fold [String] Val
toFold (Sum expr) = F.Fold add 0 Dbl where
  add x str = x + maybe 0 asDbl (eval expr str)
toFold (Distrib expr aggr) = F.Fold ins Map.empty (Map . fmap extract) where
  ins m str = case eval expr str of
    Just k -> Map.alter (Just . (\(F.Fold f x r) -> F.Fold f (f x str) r) . maybe (toFold aggr) id) k m
    Nothing -> m
toFold Len = Dbl <$> F.genericLength
toFold (Expr e) = runExpr toFold e

runExpr :: (Applicative f) => (a -> f Val) -> Expr a -> f Val
runExpr k (Val a) = k a
runExpr k (Add a b) = withDbl (+) <$> runExpr k a <*> runExpr k b
runExpr k (Sub a b) = withDbl (-) <$> runExpr k a <*> runExpr k b
runExpr k (Mul a b) = withDbl (*) <$> runExpr k a <*> runExpr k b
runExpr k (Div a b) = withDbl (/) <$> runExpr k a <*> runExpr k b
runExpr k (Pow a b) = withDbl (**) <$> runExpr k a <*> runExpr k b
runExpr k (Abs a) = Dbl . abs . asDbl <$> runExpr k a

asDbl :: Val -> Double
asDbl (Dbl a) = a
asDbl _ = error "Expecting Double"

withDbl :: (Double -> Double -> Double) -> Val -> Val -> Val
withDbl f (Dbl a) (Dbl b) = Dbl $ f a b
withDbl _ _ _ = error "Expecting Double"

eval :: Expr Prim -> [String] -> Maybe Val
eval e0 xs = runExpr (\e -> case e of
  PCol i -> Dbl <$> readMaybe (xs !! (i - 1))
  PBucket i x0 x1 ex -> case eval ex xs of
    Just (Dbl x)
      | x >= x0, x <= x1 -> let n = floor $ (x - x0) / i in Just $ Bucket
        $ MkBucket n (fromIntegral n * i + x0)
      | otherwise -> Nothing
    _ -> error "Expecting Double"
  PList es -> List <$> traverse (`eval` xs) es
  PDbl v -> Just $ Dbl v) e0

showVal :: Val -> String
showVal (Dbl a) = show a
showVal (Bucket (MkBucket _ x)) = show x
showVal (List xs) = unwords (map showVal xs)
showVal (Map m) = unlines [showVal k ++ " " ++ showVal v | (k, v) <- Map.toList m ]

main :: IO ()
main = do
  args <- getArgs
  case traverse (parseString (Expr <$> parseExpr parseAggregation <* eof) mempty) args of
    Success xs -> getContents >>= putStrLn . intercalate "," . map showVal
      . F.fold (traverse toFold xs) . map (splitOn ",") . lines
    Failure e -> hPutDoc stderr $ _errDoc e <> hardline
