test cases
testWrapperFindMultiByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self subject button). 
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self subject panelButton).