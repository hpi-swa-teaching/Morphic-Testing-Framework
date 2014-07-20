interacting
mouseUpForButton: aButtonIdentifier with: modifiers

	self morphs do: [ :morph |
		morph mouseUp: (self generateMouseEvent: #mouseUp
			button: aButtonIdentifier 
			modifiers: modifiers
			on: morph).
	].