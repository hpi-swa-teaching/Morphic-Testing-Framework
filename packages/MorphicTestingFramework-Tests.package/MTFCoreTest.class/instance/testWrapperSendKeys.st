interaction tests
testWrapperSendKeys
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject textfield1}.
	self subject textfield1 newContents: ''.
	wrapper sendKeys: 'Hello'.
	self assert: wrapper text = 'Hello'.
	wrapper sendKeys: ' World!'.
	self assert: wrapper text = 'Hello World!'.
	wrapper sendKeys: '346§/=<'.
	self assert: wrapper text = 'Hello World!346§/=<'.
	wrapper sendKeys: ''.
	self assert: wrapper text = 'Hello World!346§/=<'.