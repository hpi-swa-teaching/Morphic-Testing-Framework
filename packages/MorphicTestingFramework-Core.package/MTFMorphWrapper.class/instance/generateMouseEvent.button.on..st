Event creation
generateMouseEvent: aSymbol button: aMouseButton on: aMorph
	"aSymbol can be: #mouseMove, #mouseUp, #mouseDown"
	^MouseButtonEvent new
		setType: aSymbol
		position: aMorph center
		which: 0
		buttons: aMouseButton
		hand: self
		stamp: Time millisecondClockValue.