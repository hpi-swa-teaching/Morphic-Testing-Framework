interacting
middleClickWith: modifiersValue
	
	self morphs do: [ :morph | | wrapper |
		wrapper := (self createWrapperFor: {morph}).
		wrapper middleMouseDown: modifiersValue.
		self waitShort.
		wrapper middleMouseUp: modifiersValue.
		self wait.
	].
