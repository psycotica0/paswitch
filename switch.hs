import Pacmd

main = do
	inputs <- list_inputs
	sinks <- list_sinks
	putStrLn "Done"
