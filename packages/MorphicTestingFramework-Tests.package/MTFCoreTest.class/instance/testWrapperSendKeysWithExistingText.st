test cases
testWrapperSendKeysWithExistingText
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self testForm textfield1}.
	self assert: (wrapper text = 'abc').
	wrapper sendKeys: 'test'.
	self assert: (wrapper text = 'testabc').
