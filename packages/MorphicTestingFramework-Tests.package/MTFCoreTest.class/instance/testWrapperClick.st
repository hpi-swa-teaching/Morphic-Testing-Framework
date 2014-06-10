test cases
testWrapperClick

	| buttonWrapper |
	self assert: (self subject color = Color white).
	
	buttonWrapper := MTFMorphWrapper newWith: {subject button}.
	buttonWrapper click.
	
	self assert: (subject color = Color blue).