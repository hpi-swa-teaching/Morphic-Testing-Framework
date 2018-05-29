interaction tests
testWrapperClickCtrlNoOp

	| buttonWrapper |
	self assert: self subject interactionMorph label contents = 'not pressed'.
	
	buttonWrapper := MTFMorphWrapper newWith: {subject interactionMorph}.
	buttonWrapper clickCtrl.
	
	self assert: self subject interactionMorph label contents = 'not pressed'.