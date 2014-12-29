import Pacmd

-- This finds the current default, and then takes the next one
nextSink = head . tail . dropWhile (not . sinkdefault) . cycle

main = do
	inputs <- list_inputs
	sinks <- list_sinks
	print $ fmap nextSink sinks
	putStrLn "Done"
