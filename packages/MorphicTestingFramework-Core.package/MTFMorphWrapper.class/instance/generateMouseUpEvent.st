Event creation
generateMouseUpEvent

	^MouseButtonEvent new
		setType: #mouseUp
		position: self morph center
		which: 0
		buttons: 0
		hand: self
		stamp: Time millisecondClockValue.