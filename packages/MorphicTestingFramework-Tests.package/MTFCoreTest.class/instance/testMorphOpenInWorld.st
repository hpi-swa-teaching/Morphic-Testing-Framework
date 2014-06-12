test cases
testMorphOpenInWorld

	self slowTestBy: 1000.
	self assert: (World submorphs contains: ((self subject morphs) at: 1)).