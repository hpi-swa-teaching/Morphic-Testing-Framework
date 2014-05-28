test cases
testWrapperSendTextWithExistingText
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self testForm textfield1}.
	self assert: (wrapper text = 'abc').
	wrapper sendText: 'test'.
	"What shall the user expect? Same with sendKey"
	self assert: (wrapper text = 'test').
	self assert: (wrapper text = 'abctest').
	self assert: (wrapper text = 'testabc').
