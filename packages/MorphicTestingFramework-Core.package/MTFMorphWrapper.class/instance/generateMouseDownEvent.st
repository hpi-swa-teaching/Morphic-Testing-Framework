Event creation
generateMouseDownEvent

	^MouseButtonEvent new
		setType: #mouseDown
		position: self morph center
		which: 0
		buttons: 0
		hand: self
		stamp: Time millisecondClockValue.