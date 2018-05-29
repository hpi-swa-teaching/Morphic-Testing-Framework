interaction tests
testWrapperMiddleClick

	| buttonWrapper |
	self assert: self subject interactionMorph label contents = 'not pressed'.
	
	buttonWrapper := MTFMorphWrapper newWith: {subject interactionMorph}.
	buttonWrapper middleClick.
	
	self assert: self subject interactionMorph label contents = '2'.