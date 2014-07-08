interacting
clickShift
	"TODO"	
	| aThirdOfNormal |
	aThirdOfNormal := (1/3).
	self morphs do: [ :morph |
		morph mouseDown: (self generateMouseEvent: #mouseDown on: morph).
		self wait: aThirdOfNormal.
		morph mouseUp: (self generateMouseEvent: #mouseUp on: morph).
		self wait.
	].
