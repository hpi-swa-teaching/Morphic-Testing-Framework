interaction tests
testWrapperClickCmd

	| buttonWrapper |
	self assert: (self subject interactionMorph label contents = 'not pressed').
	
	buttonWrapper := MTFMorphWrapper newWith: {subject interactionMorph}.
	buttonWrapper clickCmd.
	
	self assert: (self subject interactionMorph label contents = '68').