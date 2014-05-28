test cases
testWrapperDragFromTo

	| wrapper start delta |
	wrapper := MTFMorphWrapper newWith: {testForm}.
	start := wrapper position.
	delta := 100@30.
	wrapper dragFrom: start to: start + delta.
	self assert: (wrapper position = (start + delta)).
	
	