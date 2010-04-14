module DeriveMod(deriveMod, deriveSMod, deriveMods)
where

import Language.Haskell.TH
import Data.Char
import Control.Monad.State.Class()

deriveMod :: String -> [Dec]
deriveMod = mkModN' . mkName

mkModN' :: Name -> [Dec]
mkModN' n = 
  let f = mkName "f"
      c = mkName "c"
      m = mkName ("mod" ++ (capitalize (nameBase n)))
  in [FunD m 
         [Clause [VarP f,VarP c] 
                 (NormalB 
                    (RecUpdE (VarE c) 
                             [(n, AppE (VarE f) (AppE (VarE n) (VarE c)))])) []]]

mkModM :: Name -> Q [Dec]
mkModM d = do
  fs <- dToFs d
  let exps = concatMap mkModN' fs
  return $ exps

dToFs d = do
  TyConI (DataD _ _ _ cons _) <- reify d
  return $ concatMap getF cons

deriveMods :: Name -> Q [Dec]
deriveMods d = do
  fs1 <- mkModM d
  let fs2 = deriveSMod fs1
  return (fs1 ++ fs2)

deriveSMod :: [Dec] -> [Dec]
deriveSMod =
  concatMap toState

toState (FunD fn _) =
  let nn = mkName ('s' : capitalize (nameBase fn))
      f  = mkName "f"
  in [FunD nn
         [Clause [VarP f] 
            (NormalB 
                (AppE (VarE (mkName "modify"))
                      (AppE (VarE fn) (VarE f)))) []]]
toState _ = []

getF :: Con -> [Name]
getF (RecC _ vars) = let (names, _, _) = unzip3 vars
                     in names
getF _             = []

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

