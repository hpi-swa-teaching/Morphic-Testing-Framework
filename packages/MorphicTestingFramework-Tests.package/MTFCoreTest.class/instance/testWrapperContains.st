test cases
testWrapperContains

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: (wrapper contains: self testForm).
	self assert: (wrapper contains: self testForm submorphs first) not.