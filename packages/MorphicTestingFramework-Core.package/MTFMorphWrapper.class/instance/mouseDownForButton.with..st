interacting
mouseDownForButton: aButtonIdentifier with: modifiers

	self morphs do: [ :morph |
		morph mouseDown: (self generateMouseEvent: #mouseDown
			button: aButtonIdentifier 
			modifiers: modifiers
			on: morph).
	].