test cases
testWrappersendKeyKeystroke
	
	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self testForm textfield}.
	self testForm textfield newContents: ''.
	wrapper sendKey: #a.
	self assert: (wrapper text = 'a').
	wrapper sendKey: #G.
	self assert: (wrapper text = 'aG').
	wrapper sendKey: #*.
	self assert: (wrapper text = 'aG*').
	
