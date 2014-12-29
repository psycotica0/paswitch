import Pacmd

main = do
	inputs <- list_inputs
	sinks <- list_sinks
	print inputs
	print sinks
	putStrLn "Done"
