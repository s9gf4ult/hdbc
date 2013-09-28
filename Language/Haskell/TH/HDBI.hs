{-# LANGUAGE
  TemplateHaskell
, QuasiQuotes
  #-}

module Language.Haskell.TH.HDBI
       (
         deriveToRow
       , deriveFromRow
       ) where

-- import Control.Applicative
import Control.Monad
import Control.Applicative
import Database.HDBI.SqlValue (ToRow(..),
                               FromRow(..),
                               FromSql(..),
                               ToSql(..),
                               ConvertError(..))
import Language.Haskell.TH


-- | return constructor name and fields count, or Nothing if data constructor is
-- infix
getTParams :: String -> Name -> Q (Name, Maybe Int)
getTParams exc name = do
  tcon <- reify name
  case tcon of
    (TyConI dec) -> do
      case dec of
        (DataD _ _ vars constrs _) -> do
          checkVars vars
          case constrs of
            [con] -> getTParams' con
            _ -> fl $ "data " ++ (show name) ++ " should have exactly one constructor"

        (NewtypeD _ _ vars con _) -> do
          checkVars vars
          getTParams' con

        _ -> fl $ "deriveToRow can derive just for data with one constructor or for newtypes"
    _ -> fl $ (show name) ++ " must be a type"

  where
    fl x = fail $ exc ++ x
    checkVars [] = return ()
    checkVars _ = fl $ "type " ++ show name ++ " should not have type variables"

    getTParams' :: Con -> Q (Name, Maybe Int)
    getTParams' (NormalC n fields) = return (n, Just $ length fields)
    getTParams' (RecC n fields) = return (n, Just $ length fields)
    getTParams' (InfixC _ n _) = return (n, Nothing)
    getTParams' _ = fl $ "data constructors should not contain typevar boundries for " ++ show name


-- | Derive `ToRow` instance for any data with one constructor or for newtype
deriveToRow :: Name -> Q [Dec]
deriveToRow name = do
  (con, fields) <- getTParams "deriveToRow: " name
  names <- case fields of
    Just fl -> replicateM fl $ newName "val"
    Nothing -> replicateM 2 $ newName "val"

  return [InstanceD [] (AppT (ConT ''ToRow) (ConT name))
          [FunD 'toRow
           [Clause [mkPattern con fields names]
           (NormalB $ ListE $ map (\nm -> AppE (VarE 'toSql) (VarE nm)) names) [] ]]]
  where
    mkPattern con Nothing [n1, n2] = InfixP (VarP n1) con (VarP n2)
    mkPattern con (Just _) names = ConP con $ map VarP names


deriveFromRow :: Name -> Q [Dec]
deriveFromRow name = do
  (con, fields) <- getTParams "deriveFromRow: " name
  names <- case fields of
    Just fl -> replicateM fl $ newName "val"
    Nothing -> replicateM 2 $ newName "val"
  xname <- newName "x"
  return [InstanceD [] (AppT (ConT ''FromRow) (ConT name))
          [FunD 'safeFromRow
           [Clause [ListP $ map VarP names]
            (NormalB $ UInfixE (mkCon fields con) (VarE '(<$>)) (foldedFromSql names)) []
           ,Clause [VarP xname]
            (NormalB $ AppE (ConE 'Left) (AppE (ConE 'ConvertError)
             (UInfixE
              (LitE $ StringL $ "Could not construct " ++ show name
                ++ ": query must return exactly "
                ++ (show $ length names) ++ " values but not " )
              (VarE '(++))
              (AppE (VarE 'show) (AppE (VarE 'length) (VarE xname)))))) []]]]

  where
    foldedFromSql names = foldl1 (\a b -> UInfixE a (VarE '(<*>)) b)
                           $ map (\n -> AppE (VarE 'safeFromSql) (VarE n)) names

    mkCon (Just _) con = ConE con
    mkCon Nothing con = ParensE $ ConE con
