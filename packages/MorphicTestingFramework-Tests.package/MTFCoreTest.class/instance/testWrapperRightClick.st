interaction tests
testWrapperRightClick

	| buttonWrapper |
	self assert: (self subject interactionMorph label contents = 'not pressed').
	
	buttonWrapper := MTFMorphWrapper newWith: {subject interactionMorph}.
	buttonWrapper clickShift.
	
	self assert: (self subject interactionMorph label contents = '1').