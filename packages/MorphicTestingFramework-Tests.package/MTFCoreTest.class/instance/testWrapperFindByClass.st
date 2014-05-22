test cases
testWrapperFindByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self testForm button).