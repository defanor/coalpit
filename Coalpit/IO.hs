{- |
Module      :  Coalpit.IO
Description :  Helper IO functions
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

These are basic utility functions for pipes-based IO.

An example:

@
\{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}
import GHC.Generics
import Pipes.Prelude as PP
import Coalpit

data Args = Args { arg1 :: Maybe Int, arg2 :: Double }
  deriving (Generic, Coalpit)
data Input = Input Double deriving (Generic, Coalpit)
data Output = Foo Double | Bar deriving (Generic, Coalpit)

main :: IO ()
main = run' $ \a -> PP.mapM $ \(Input i) ->
  pure $ Foo $ maybe (arg2 a) fromIntegral (arg1 a) + i
@

-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Coalpit.IO (run, run', handleErrors) where

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
run :: forall m a i o. (MonadIO m, Coalpit a, Coalpit i, Coalpit o)
    => (String -> m [i])
    -- ^ An action to run on error (see 'handleErrors').
    -> (a -> Pipe i o m ())
    -- ^ Main function.
    -> m ()
run e f = do
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

-- | Same as 'run', but just prints errors into 'stderr'.
run' :: forall m a i o. (MonadIO m, Coalpit a, Coalpit i, Coalpit o)
         => (a -> Pipe i o m ())
         -- ^ Main function.
         -> m ()
run' = run (\e -> liftIO $ hPutStrLn stderr e >> pure [])
