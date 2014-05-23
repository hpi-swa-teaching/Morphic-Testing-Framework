test cases
testWrapperFindMultiByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: {
		self testForm button. 
		self testForm panelButton}).