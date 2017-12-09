module TypeCheck where

import Control.Monad.Except
import Data.Either
import Control.Monad.Reader
import qualified Data.Map as Map
import AbsLatte


type TCMonad = ReaderT TCEnv (Except String)

type TCEnv = Map.Map Ident (Either Type (Map.Map Ident Type))

prepareTopEnv :: [TopDef] -> TCEnv
prepareTopEnv [] = startEnv
prepareTopEnv (def:rest) = 
    let (ident, val) = case def of {
        FnDef t ident args _ -> (ident, Left $ Fun t (map a2t args));
        ClDef ident members -> (ident, Right $ prepClsTopEnv members);
        DepClDef ident anc members -> (ident, Right $ prepClsTopEnv members)}
    in Map.insert ident val $ prepareTopEnv rest


-- STAART HERE
prepareTCMonad :: [TopDef] -> TCMonad ()
prepareTCMonad [] = return ()
--prepareTCMonad (def:rest) = case def of {
--    FnDef t ident args body -> return () -- TODO
--    ClDef ident members -> 
--}

prepClsTopEnv :: [MemberDef] -> Map.Map Ident Type
prepClsTopEnv [] = Map.empty
prepClsTopEnv (def:rest) = case def of
    MAttr t items -> foldr (flip Map.insert $ t) (prepClsTopEnv rest) $ map i2id items --TODO is foldl faster?
    MMethod t id args _ -> Map.insert id (Fun t $ map a2t args) $ prepClsTopEnv rest
 
startEnv :: TCEnv
startEnv = Map.empty --TODO dodać printy i ready

a2t :: Arg -> Type
a2t (Arg t _) = t

i2id :: Item -> Ident
i2id it = case it of
    NoInit id -> id
    Init id _ -> id
    InitAlloc id _ -> id
