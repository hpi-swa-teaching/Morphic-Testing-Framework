interacting
leftClickWith: modifiers
	
	self morphs do: [ :morph | |wrapper |
		wrapper := self createWrapperFor: {morph}.
		wrapper leftMouseDown: modifiers.
		self waitShort.
		wrapper leftMouseUp: modifiers.
		self wait].
