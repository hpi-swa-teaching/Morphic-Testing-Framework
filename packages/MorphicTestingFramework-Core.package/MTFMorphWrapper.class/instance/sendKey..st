interacting
sendKey: aCharacter

	self morphs do: [ :morph |
		morph keyStroke: 
			(self generateKeyboardEvent: #keystroke with: aCharacter on: morph).	
	].