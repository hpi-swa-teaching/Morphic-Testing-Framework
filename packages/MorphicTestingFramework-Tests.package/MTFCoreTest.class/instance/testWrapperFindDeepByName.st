test cases
testWrapperFindDeepByName

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByName: 'button_in_panel') contains: self subject panelButton).