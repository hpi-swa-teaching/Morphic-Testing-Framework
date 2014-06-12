interaction tests
testWrapperSendKeyKeystroke
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject textfield1}.
	self subject textfield1 newContents: ''.
	wrapper sendKey: $a.
	self assert: (wrapper text = 'a').
	wrapper sendKey: $G.
	self assert: (wrapper text = 'aG').
	wrapper sendKey: $*.
	self assert: (wrapper text = 'aG*').
	wrapper sendKey: $3.
	self assert: (wrapper text = 'aG*3').
	
