interacting
mouseUp: modifiers

	self morphs do: [ :morph |
		morph mouseUp: (self generateMouseEvent: #mouseUp 
			button: MTFMorphWrapper leftButton 
			modifiers: modifiers
			on: morph)
	].