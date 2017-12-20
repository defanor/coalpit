{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveGeneric,
  DeriveAnyClass #-}

module Coalpit.IO (runMain, runMain', handleErrors) where

import Data.Proxy (Proxy(..))
import System.Environment (getProgName, getArgs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Pipes ((>->), Pipe, yield, await, lift, runEffect)
import Control.Monad (mapM_, forever)
import qualified Pipes.Prelude as PP
import Coalpit.Core (Coalpit, fromArgs, defOpt, usage)
import Coalpit.DSV (readDSV, showDSV)
import GHC.Generics

-- | Runs a given action on each 'Left' value, embedding that action's
-- result into the data stream.
handleErrors :: MonadIO m => (e -> m [a]) -> Pipe (Either e a) a m ()
handleErrors e = forever $ do
  v <- await
  case v of
    Left err -> do
      vs <- lift $ e err
      mapM_ yield vs
    Right x -> yield x

-- | Runs a given 'Pipe' between input producer and output consumer.
-- Prints an error and usage instructions if it fails to parse the
-- arguments, and passes the input through 'handleErrors'.
runMain :: forall m a i o. (MonadIO m, Coalpit a, Coalpit i, Coalpit o)
        => (String -> m [i])
        -- ^ An action to run on error (see 'handleErrors').
        -> (a -> Pipe i o m ())
        -- ^ Main function.
        -> m ()
runMain e f = do
  pn <- liftIO getProgName
  let u = Prelude.concat ["Usage: ", pn, " ", usage defOpt (Proxy :: Proxy a)]
  args <- liftIO getArgs
  a <- either (liftIO . die . (++ u)) pure $ fromArgs defOpt args
  runEffect $
    PP.stdinLn
    >-> PP.map (readDSV defOpt)
    >-> handleErrors e
    >-> f a
    >-> PP.map (showDSV defOpt)
    >-> PP.stdoutLn

-- | Same as 'runMain', but just prints errors into 'stderr'.
runMain' :: forall m a i o. (MonadIO m, Coalpit a, Coalpit i, Coalpit o)
         => (a -> Pipe i o m ())
         -- ^ Main function.
         -> m ()
runMain' = runMain (\e -> liftIO $ hPutStrLn stderr e >> pure [])


data Args = Args { arg1 :: Maybe Int, arg2 :: Double }
  deriving (Generic, Coalpit)
data Input = Input Double deriving (Generic, Coalpit)
data Output = Foo Double | Bar deriving (Generic, Coalpit)

main :: IO ()
main = runMain' $ \a -> PP.mapM $ \(Input i) ->
  pure $ Foo $ maybe (arg2 a) fromIntegral (arg1 a) + i
