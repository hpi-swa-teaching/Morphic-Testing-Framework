search tests
testWrapperFindDeepByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self subject panelButton).