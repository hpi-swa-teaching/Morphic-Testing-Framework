interacting
click
	
	self morphs do: [ :morph |
		morph
			mouseDown: (self generateMouseEvent: #mouseDown on: morph);
			mouseUp: (self generateMouseEvent: #mouseUp on: morph).
		self wait.
	].
