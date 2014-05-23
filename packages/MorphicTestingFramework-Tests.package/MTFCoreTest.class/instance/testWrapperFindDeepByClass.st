test cases
testWrapperFindDeepByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self testForm panelButton).