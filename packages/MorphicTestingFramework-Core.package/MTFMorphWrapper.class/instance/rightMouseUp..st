interacting
rightMouseUp: modifiers

	self morphs do: [ :morph |
		morph mouseUp: (self generateMouseEvent: #mouseUp
			button: MTFMorphWrapper rightButton 
			modifiers: modifiers
			on: morph).
	].