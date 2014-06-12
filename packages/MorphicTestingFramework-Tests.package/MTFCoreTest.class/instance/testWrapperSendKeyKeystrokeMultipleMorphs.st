interaction tests
testWrapperSendKeyKeystrokeMultipleMorphs
	
	| wrapper textfields |
	textfields := Set with: (self subject textfield1) with: (self subject textfield2) with: (self subject textfield3).
	wrapper := MTFMorphWrapper newWith: textfields.
	self subject textfield1 newContents: ''.
	self subject textfield2 newContents: ''.
	self subject textfield3 newContents: ''.
	wrapper sendKey: $a.
	self assert: (self subject textfield1 text = 'a').
	self assert: (self subject textfield2 text = 'a').
	self assert: (self subject textfield3 text = 'a').
