search tests
testWrapperFindByClass

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByClass: SimpleButtonMorph) contains: self subject button).