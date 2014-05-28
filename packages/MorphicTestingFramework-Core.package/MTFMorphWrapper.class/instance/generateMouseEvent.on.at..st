Event creation
generateMouseEvent: aSymbol on: aMorph at: aPosition
	"aSymbol can be: #mouseMove, #mouseUp, #mouseDown"
	^MouseButtonEvent new
		setType: aSymbol
		position: aPosition
		which: 0
		buttons: 0
		hand: self
		stamp: Time millisecondClockValue.