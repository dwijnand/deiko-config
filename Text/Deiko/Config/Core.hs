{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Deiko.Config.Core (loadConfig
                              ,getValue
                              ,getValues
                              ,HasConfig(..)
                              ,CanReport(..)
                              ,ConfigError(..)) where

import           Control.Applicative        (WrappedMonad (..))
import           Control.Monad              (liftM)
import           Control.Monad.Reader       (MonadReader, asks, runReaderT)
import           Control.Monad.State        (execStateT, get, put)
import           Control.Monad.Trans        (MonadIO (..), lift)

import           Data.Foldable              (traverse_)
import           Data.List                  (unionBy)
import qualified Data.Map                   as M
import           Data.Traversable           (traverse)

import           Text.Deiko.Config.Semantic
import           Text.Deiko.Config.Types

loadConfig :: (CanReport m, MonadIO m) => String -> m Config
loadConfig path =
  do file <- liftIO $ readFile path
     either reportError return (compile file)

getValue :: (ConfigValue v, CanReport m, HasConfig r, MonadReader r m)
         => String
         -> m v
getValue key =
  getEnv (transform configValue defaultSubstHandler key)
           (reportError . propErrMsg) key

getValues :: (ConfigValue v, CanReport m, HasConfig r, MonadReader r m)
          => String
          -> m [v]
getValues key =
  getEnv (transform go defaultSubstHandler key)
           (reportError . propErrMsg) key
  where
    go key (PLIST xs) =
      unwrapMonad (traverse (WrapMonad . configValue key) xs)
    go key x = reportError $ expErrMsg key "List" (showType x)

getEnv :: (HasConfig r, CanReport m, MonadReader r m)
       => (Register -> PropValue -> m v)
       -> (String -> m v)
       -> String
       -> m v
getEnv onSuccess onError key =
  do register <- asks (configRegister . getConfig)
     maybe (onError key) (onSuccess register) (M.lookup key register)

transform :: CanReport m
          => (String -> PropValue -> m v)
          -> (String -> String -> m v) -- on susbstitution
          -> String
          -> Register
          -> PropValue
          -> m v
transform f onsub key reg (PSUBST sub) =
  case sub of
    ('?':skey) -> maybe (transform f onsub key reg (PSTRING ""))
                  (transform f onsub key reg)
                  (M.lookup skey reg)
    _          -> maybe (onsub key sub)
                  (transform f onsub key reg)
                  (M.lookup sub reg)
transform f onsub key reg (PCONCAT (x:xs)) =
  transform f onsub key reg =<< action
    where
      action = execStateT (unwrapMonad $ traverse_ (WrapMonad . concat) xs) x

      concat v =
        do acc <- get
           v1  <- lift $ transform (const return) defaultSubstHandler key reg v
           put (merge acc v1)

      merge (PSTRING x) (PSTRING y)   = PSTRING (x ++ " " ++ y)
      merge (PLIST xs) (PLIST ys)     = PLIST (xs ++ ys)
      merge (POBJECT (Object xs)) (POBJECT (Object ys)) =
        let f x y = (str $ propName x) == (str $ propName y)
            str (PSTRING x) = x in
        POBJECT (Object $ unionBy f ys xs)
transform f onsub key reg (PLIST xs) =
  f key =<< action
  where
    action = unwrapMonad $ fmap PLIST (traverse go xs)

    go = WrapMonad . transform constM defaultSubstHandler key reg

    constM = const return
transform f _ key _ x = f key x

defaultSubstHandler :: CanReport m => String -> String -> m a
defaultSubstHandler key sub = reportError $ substErrMsg key sub
