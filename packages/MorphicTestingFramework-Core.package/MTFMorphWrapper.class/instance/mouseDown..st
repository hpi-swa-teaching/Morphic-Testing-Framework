interacting
mouseDown: modifiers

	self morphs do: [ :morph |
		morph mouseDown: (self generateMouseEvent: #mouseDown 
			button: MTFMorphWrapper leftButton 
			modifiers: modifiers
			on: morph).
	].