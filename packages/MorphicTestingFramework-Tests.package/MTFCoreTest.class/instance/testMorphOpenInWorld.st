test cases
testMorphOpenInWorld

	self slowTestBy: 500.
	self assert: (World submorphs contains: [:morph | morph == ((self subject morphs) at: 1)]).