Event creation
generateMouseEvent: aSymbol on: aMorph
	"aSymbol can be: #mouseMove, #mouseUp, #mouseDown"
	^MouseButtonEvent new
		setType: aSymbol
		position: aMorph center
		which: 0
		buttons: 0
		hand: self
		stamp: Time millisecondClockValue.