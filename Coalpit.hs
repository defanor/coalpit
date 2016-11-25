{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Coalpit where

import Language.Haskell.TH
import Data.Char
import Data.List


class Args a where
  toArgs :: a -> [String]
  fromArgs :: [String] -> (a, [String])

instance {-#OVERLAPPING#-} Args String where
  toArgs = pure . show
  fromArgs (x:xs) = (read x, xs)

instance Args Int where
  toArgs = pure . show
  fromArgs (x:xs) = (read x, xs)

instance Args Bool where
  toArgs True = ["t"]
  toArgs False = ["f"]
  fromArgs ("t":xs) = (True, xs)
  fromArgs ("f":xs) = (False, xs)

instance Args a => Args (Maybe a) where
  toArgs Nothing = ["n"]
  toArgs (Just x) = "j" : toArgs x
  fromArgs ("n":xs) = (Nothing, xs)
  fromArgs ("j":xs) = let (y, ys) = fromArgs xs in (Just y, ys)

instance Args a => Args [a] where
  toArgs = pure . intercalate "," . concatMap toArgs
  fromArgs (arg:args) = (map (fst . fromArgs . pure) $ splitOn ',' arg, args)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep l = case break (== sep) l of
  (x, []) -> [x]
  (x, _:xs) -> x : splitOn sep xs

printCon :: Name -> String
printCon = map toLower . reverse . takeWhile (/= '.') . reverse . show

printClause :: Con -> Q Clause
printClause (NormalC cn bts) = do
  let ts = map snd bts
      vars = (map (mkName . ("x_" ++) . show) [1..length ts])
      args = map (\v -> AppE (VarE 'toArgs) (VarE v)) vars
  pure $ Clause [ConP cn (map VarP vars)]
    (NormalB (AppE (VarE 'concat) $ ListE
               (ListE [LitE (StringL $ printCon cn)] : args)))
    []

parseClause :: Con -> Q Clause
parseClause (NormalC cn bts) = do
  let ts = map snd bts
      vars = map (mkName . ("x_" ++) . show) [1..length ts]
      vals = map (\x -> mkName (show x ++ "_val")) vars
      fin = (TupE [foldl AppE (ConE cn) (map VarE vals)
                   , VarE (mkName $ (if length vars > 0
                                     then show (last vars)
                                     else "xs") ++ "_rest")
                   ])
  pure $ Clause [InfixP (LitP (StringL $ printCon cn))
                 (mkName ":")
                 (VarP (mkName "xs_rest"))]
    (NormalB $ foldr parseArg fin (zip (mkName "xs" : vars) vars))
    []
  where
    parseArg :: (Name, Name) -> Exp -> Exp
    parseArg (np, nc) e = LetE
      [ValD (TupP [VarP (mkName (show nc ++ "_val")),
                    VarP (mkName (show nc ++ "_rest"))])
        (NormalB $ AppE (VarE 'fromArgs) (VarE (mkName (show np ++ "_rest"))))
        []]
      e

deriveArgs :: Name -> Q [Dec]
deriveArgs ty = do
  let var = mkName "x"
  (TyConI d@(DataD _ nm tyVars mk cs _)) <- reify ty
  to <- mapM printClause cs
  from <- mapM parseClause cs
  pure [InstanceD Nothing [] (AppT (ConT ''Args) (ConT ty))
        [FunD 'toArgs to, FunD 'fromArgs from]]
