module Enigma( 
		cifrar
	)
	where
		import Data.Char
		type Discos = (String, String, String, String)
		type EstadoEnigma = (Int, Int, Int)
		cifrar :: Discos -> EstadoEnigma -> String -> (String, EstadoEnigma)
		cifrar d e s = cifra d e mensaje 
			where
				mensaje = filter (isUpper) $ map toUpper s

		siguiente :: Int -> Int
		siguiente c = mod (c+1) 26


		estadoSiguiente (25,25,25) = (0,0,0)
		estadoSiguiente (e1,25,25) = (siguiente e1,0,0)
		estadoSiguiente (e1,e2,25) = (e1,siguiente e2,0)
		estadoSiguiente (e1,e2,e3) = (e1,e2,siguiente e3)

		cifra _ e [] = ([],e)
		cifra d e (c:t)  = (x:r, e')
			where 
				x = cifraLetra d e c
				(r,e') = cifra d (estadoSiguiente e) t 

		cifraLetra (d1,d2,d3,d4) (e1,e2,e3) l = c
			where
				c  = cifraDisco' d1 e1 $
					cifraDisco' d2 e2 $
					cifraDisco' d3 e3 $
					cifraDisco d4 0  $
					cifraDisco d3 e3  $ 
					cifraDisco d2 e2  $
					cifraDisco d1 e1  l

		cifraDisco d e l = c
			where
				i = (e + indiceAlfabeto l) `mod` 26
				c = d !! i

		indiceAlfabeto c = ord c - ord 'A' 

		cifraDisco' d e l = c 
			where
				c = find ['A'..'Z'] (\x -> l == cifraDisco d e x )

		find (x:t) f 
			| f x 		= x
			| otherwise = find t f
