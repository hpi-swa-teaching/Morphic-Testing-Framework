test cases
testWrapperClick

	| buttonWrapper |
	self assert: (self testForm color = Color white).
	
	buttonWrapper := MTFMorphWrapper newWith: {testForm button}.
	buttonWrapper click.
	
	self assert: (testForm color = Color blue).