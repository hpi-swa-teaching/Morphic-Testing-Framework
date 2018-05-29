interaction tests
testWrapperClick

	| buttonWrapper |
	self assert: self subject interactionMorph label contents = 'not pressed'.
	
	buttonWrapper := MTFMorphWrapper newWith: {subject interactionMorph}.
	buttonWrapper click.
	
	self assert: self subject interactionMorph label contents = '4'.