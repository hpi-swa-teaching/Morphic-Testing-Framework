interacting
sendMouseEvent: aType button: aButton modifiers: modifiers

	self morphs do: [ :morph |
		self hand newMouseFocus: morph.
		morph handleEvent: (self generateMouseEvent: aType 
			button: aButton
			modifiers: modifiers
			on: morph).
	].