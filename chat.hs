import Graphics.UI.WX hiding (bracket)
import Graphics.UI.WXCore hiding (bracket)
import Network.Socket
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.Environment
import Data.Maybe (fromJust)
import Enigma

sendDatagram msg port ip = do
	(serveraddr:_) <- getAddrInfo Nothing (Just ip) (Just port)
	s <- socket (addrFamily serveraddr) Datagram defaultProtocol
	sock <-connect s (addrAddress serveraddr) >> return s
	send sock msg
	Network.Socket.close sock

listenPort port d1TB d2TB d3TB d4TB estadosd1 estadosd2 estadosd3 = withSocketsDo $ bracket getSocket sClose handler
	where 
		getSocket = do
			(serveraddr:_) <- getAddrInfo
				(Just (defaultHints {addrFlags = [AI_PASSIVE]}))
				Nothing (Just port)
			sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
			bindSocket sock (addrAddress serveraddr) >> return sock
		handler conn = do
			(msg,n,d) <- recvFrom conn 1024
			d1 <- get d1TB text
			d2 <- get d2TB text
			d3 <- get d3TB text
			d4 <- get d4TB text
			e1 <- get estadosd1 selection 
			e2 <- get estadosd2 selection 
			e3 <- get estadosd3 selection 
			let (msg', (e'1,e'2, e'3))  = (cifrar (d1,d2,d3,d4) (e1,e2,e3)) msg
			set estadosd1 [selection := e'1] 
			set estadosd2 [selection := e'2] 
			set estadosd3 [selection := e'3] 
			logMessage $ (takeWhile (/=':') $ show d) ++ ":\t" ++ msg' 
			if null msg then handler conn else sendTo conn msg d >> handler conn

main = do
	(puertoEnt:args) <- getArgs
	start (gui puertoEnt)

gui puertoEnt= do
	f <- frame [text:= "Chat v1"]
	p <- panel f []
	
	--Direcciones
	direcciones <- panel p []
	ipl 		<- staticText direcciones [text:= "IP:"]
	puertoSall  <- staticText direcciones [text:= "Puerto Destino:"]
	puertoEntl  <- staticText direcciones [text:= "Puerto Entrada:"]

	ipTB 		<- textEntry direcciones [enabled := True, wrap := WrapNone]
	psTB		<- textEntry direcciones [enabled := True, wrap := WrapNone]
	peTB		<- textEntry direcciones [enabled := False, wrap := WrapNone, text := puertoEnt]

	--Enigma
	enigma1	<- panel p []
	enigma2	<- panel p []
	enigma3		<- panel p []
	
	d1l	 	<- staticText enigma1 [text := "D1:"]
	d2l	 	<- staticText enigma1 [text := "D2:                "]
	d3l	 	<- staticText enigma2 [text := "D3:"]
	revl 	<- staticText enigma2 [text := "Reflector:"]
	
	d1TB	<- textEntry enigma1 [enabled := True, wrap := WrapNone, text:= "EKMFLGDQVZNTOWYHXUSPAIBRCJ"]
	d2TB	<- textEntry enigma1 [enabled := True, wrap := WrapNone, text:= "AJDKSIRUXBLHWTMCQGZNPYFVOE"]
	d3TB	<- textEntry enigma2 [enabled := True, wrap := WrapNone, text:= "BDFHJLCPRTXVZNYEIWGAKMUSQO"]
	revTB	<- textEntry enigma2 [enabled := True, wrap := WrapNone, text:= "YRUHQSLDPXNGOKMIEBFZCWVJAT"]
	
	let alfabeto = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"]

	checkEngmtxt <- textCtrl enigma3 [enabled := False, wrap := WrapNone]
	checkEngmbtn <- button 	 enigma3 [enabled := True , text := "Comprobar", on command := set checkEngmtxt [enabled :~ not]]
	estadosd1 	 <- singleListBox enigma3 [enabled := True, items :=  alfabeto, clientSize := (sz 10 10)] 
	estadosd2 	 <- singleListBox enigma3 [enabled := True, items :=  alfabeto] 
	estadosd3 	 <- singleListBox enigma3 [enabled := True, items :=  alfabeto] 
	estadosrev 	 <- singleListBox enigma3 [enabled := True, items :=  alfabeto] 

	--Chat Log
	chatlog <- textCtrl p [enabled := False, wrap := WrapNone]
	textCtrlMakeLogActiveTarget chatlog

	--Mensaje 
	mensajes	<- panel f []
	mensaje 	<- textCtrl mensajes [enabled := True, wrap := WrapNone]
	msjbtn		<- button mensajes   [enabled := True, text := "Enviar", on command := do 
		d1 <- get d1TB text
		d2 <- get d2TB text
		d3 <- get d3TB text
		d4 <- get revTB text
		e1 <- get estadosd1 selection 
		e2 <- get estadosd2 selection 
		e3 <- get estadosd3 selection 
		msg' <- get mensaje text
		(msg, (e'1,e'2, e'3))  <- fmap (cifrar (d1,d2,d3,d4) (e1,e2,e3)) $ get mensaje text
		port <- get psTB text
		ip <- get ipTB text
		unless (null msg) $ do
			logMessage ("Tú:\t\t\t" ++ msg')
			sendDatagram msg port ip
		set mensaje [text := ""]
		set estadosd1 [selection := e'1] 
		set estadosd2 [selection := e'2] 
		set estadosd3 [selection := e'3] 
		return ()]


	
	--Para debuggear
	set ipTB [text := "localhost"]
	set psTB [text := "9999"]

	--Ensamblado ventana
	set checkEngmtxt [clientSize := (sz 10 200)]
	h1 <- forkOS (listenPort puertoEnt d1TB d2TB d3TB revTB estadosd1 estadosd2 estadosd3)

	set f [ layout := 
			hfill $ container p $ margin 10 $ floatCentre $ column 6 [
				container direcciones 	$ margin 10 $ row 6 [widget ipl, widget ipTB, widget puertoSall, widget psTB, widget puertoEntl, widget peTB],
				container enigma1 		$ margin 10 $ row 4 [widget d1l , widget d1TB, widget d2l, widget d2TB],
				container enigma2 		$ margin 10 $ row 4 [widget d3l, widget d3TB, widget revl, widget revTB],
				container enigma3 		$ margin 10 $ row 6 [widget checkEngmbtn, widget checkEngmtxt,widget estadosrev, widget estadosd1, widget estadosd2, widget estadosd3],
				hfill $ minsize (sz 40 200) $ widget chatlog,
				hfill $ container mensajes 	$ margin 10 $ row 2 [hfill $ widget mensaje, widget msjbtn]
			]
		]
	return ()

