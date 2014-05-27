interacting
sendKey: aSymbol

	self morphs do: [ :morph |
		morph keyStroke: (self generateKeyboardEvent: #keystroke with: aSymbol on: morph).	
	].