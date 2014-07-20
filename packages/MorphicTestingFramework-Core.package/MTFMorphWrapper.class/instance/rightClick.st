interacting
rightClick
	
	| aThirdOfNormal |
	aThirdOfNormal := (1/3).
	self morphs do: [ :morph |
		morph mouseDown: 
			(self generateMouseEvent: #mouseDown button: MTFUtil rightButton on: morph).
		self wait: aThirdOfNormal.
		morph mouseUp: 
			(self generateMouseEvent: #mouseUp button: MTFUtil rightButton on: morph).
		self wait.
	].
