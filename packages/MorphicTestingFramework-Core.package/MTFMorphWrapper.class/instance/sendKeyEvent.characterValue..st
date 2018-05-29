interacting
sendKeyEvent: aType characterValue: aValue
	
	self morphs do: [ :morph |
		self hand newKeyboardFocus: morph.
		morph handleEvent: (self generateKeyboardEvent: aType with: aValue on: morph).	
		self wait].