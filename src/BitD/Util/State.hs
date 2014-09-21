module BitD.Util.State ( modify'
                       ) where

import qualified Control.Monad.State as State

modify' :: State.MonadState s m => (s -> s) -> m ()
modify' f = do v <- State.get
               State.put $! f v
