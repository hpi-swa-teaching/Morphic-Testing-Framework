interacting
middleClick
	
	| aThirdOfNormal |
	aThirdOfNormal := (1/3).
	self morphs do: [ :morph |
		morph mouseDown: 
			(self generateMouseEvent: #mouseDown button: MTFUtil middleButton on: morph).
		self wait: aThirdOfNormal.
		morph mouseUp: 
			(self generateMouseEvent: #mouseUp button: MTFUtil middleButton on: morph).
		self wait.
	].
