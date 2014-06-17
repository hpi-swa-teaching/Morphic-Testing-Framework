interacting
click
	
	self morphs do: [ :morph |
		morph mouseDown: (self generateMouseEvent: #mouseDown on: morph).
		self wait: (1/3).
		morph mouseUp: (self generateMouseEvent: #mouseUp on: morph).
		self wait.
	].
