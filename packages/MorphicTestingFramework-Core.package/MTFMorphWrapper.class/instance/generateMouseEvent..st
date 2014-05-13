Event creation
generateMouseEvent: aSymbol
	"aSymbol can be: #mouseMove, #mouseUp, #mouseDown"
	^MouseButtonEvent new
		setType: aSymbol
		position: self morph center
		which: 0
		buttons: 0
		hand: self
		stamp: Time millisecondClockValue.