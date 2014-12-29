import Pacmd
import Control.Monad.Trans.Error (ErrorT(..), runErrorT, Error, strMsg)
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Error (ParseError, newErrorMessage, Message(..))
import Control.Monad.Trans.Class (lift)

instance Error ParseError where
	strMsg str = newErrorMessage (Message str) $ initialPos "Error"

-- This finds the current default, and then takes the next one
nextSink = head . tail . dropWhile (not . sinkdefault) . cycle

switchDefault = do
	next_sink <- fmap nextSink $ ErrorT list_sinks
	lift $ putStrLn $ "Switching default sink to " ++ sinkname next_sink
	lift $ set_default_sink next_sink
	return next_sink

moveInputs sink = ErrorT list_inputs >>= lift . (mapM_ $ flip move_input sink)

main = handle =<< (runErrorT $ switchDefault >>= moveInputs)
	where
	handle (Left err) = putStrLn $ "Error:\n" ++ show err
	handle (Right _) = return ()
