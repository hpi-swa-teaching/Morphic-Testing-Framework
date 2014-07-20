interacting
middleMouseUp: modifiers

	self morphs do: [ :morph |
		morph mouseUp: (self generateMouseEvent: #mouseUp 
			button: MTFMorphWrapper middleButton
			modifiers: modifiers
			on: morph).
	].