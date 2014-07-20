interacting
rightClickwith: modifiersValue
	
	self morphs do: [ :morph | |wrapper |
		wrapper := (self createWrapperFor: {morph}).
		wrapper rightMouseDown: modifiersValue.
		self waitShort.
		wrapper rightMouseUp: modifiersValue.
		self wait.
	].
