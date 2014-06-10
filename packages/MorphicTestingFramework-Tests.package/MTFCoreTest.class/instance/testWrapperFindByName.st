test cases
testWrapperFindByName

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByName: 'button_change_color') contains: self subject button).