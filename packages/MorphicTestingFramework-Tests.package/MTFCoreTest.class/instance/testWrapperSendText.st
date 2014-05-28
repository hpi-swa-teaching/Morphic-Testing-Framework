test cases
testWrapperSendText
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self testForm textfield1}.
	self testForm textfield1 newContents: ''.
	wrapper sendText: 'Hello'.
	self assert: (wrapper text = 'Hello').
	wrapper sendText: ' World!'.
	self assert: (wrapper text = 'Hello World!').
	wrapper sendText: '346§/=<'.
	self assert: (wrapper text = 'Hello World!346§/=<').
	wrapper sendText: ''.
	self assert: (wrapper text = 'Hello World!346§/=<').