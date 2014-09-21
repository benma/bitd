module BitD.Prelude ( liftIO
                    , lift
                    , debugM, infoM, noticeM, warningM, errorM
                    , point, break'
                    , fix
                    , mconcat
                    , fromJust
                    , Int32, Int64
                    , Word32, Word64
                    , MonadIO, MonadTrans
                    , module Control.Applicative
                    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trans.Class (lift)

import System.Log.Logger (debugM, infoM, noticeM, warningM, errorM)
import Data.Monoid (mconcat)
import Data.Maybe (fromJust)
import Control.Applicative
import qualified Control.Monad as M
import Control.Monad.Fix (fix)
import qualified Control.Error as Err
import qualified Data.HashSet as Set
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

point :: (Functor m) => Err.EitherT e m a -> m ()
point m = M.void $ Err.runEitherT m
break' :: Monad m => Err.EitherT () m ()
break' = Err.throwT ()
