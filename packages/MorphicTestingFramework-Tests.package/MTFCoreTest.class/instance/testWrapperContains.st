test cases
testWrapperContains

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: (wrapper contains: self subject).
	self assert: (wrapper contains: self subject submorphs first) not.