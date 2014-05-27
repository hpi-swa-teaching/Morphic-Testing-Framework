Event creation
generateKeyboardEvent: aType with: aKey on: aMorph
	"type can be #keyDown, #keyUp, #keystroke"
	^KeyboardEvent new
		setType: aType
		buttons: 0
		position: aMorph center
		keyValue: aKey
		hand: self
		stamp: Time millisecondClockValue.