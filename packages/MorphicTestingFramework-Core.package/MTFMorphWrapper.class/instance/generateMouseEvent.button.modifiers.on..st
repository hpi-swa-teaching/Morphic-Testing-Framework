private-event-generation
generateMouseEvent: aSymbol button: aMouseButton modifiers: modifiersValue on: aMorph
	"aSymbol can be: #mouseMove, #mouseUp, #mouseDown"
	
	^MouseButtonEvent new
		setType: aSymbol
		position: aMorph center
		which: 0
		buttons: aMouseButton + modifiersValue
		hand: self
		stamp: Time millisecondClockValue.