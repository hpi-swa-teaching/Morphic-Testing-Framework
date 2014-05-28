test cases
testWrapperSendKeyKeystrokeMultipleMorphs
	
	| wrapper textfields |
	textfields := Set with: (self testForm textfield1) with: (self testForm textfield2) with: (self testForm textfield3).
	wrapper := MTFMorphWrapper newWith: textfields.
	self testForm textfield1 newContents: ''.
	self testForm textfield2 newContents: ''.
	self testForm textfield3 newContents: ''.
	wrapper sendKey: $a.
	self assert: (self testForm textfield1 text = 'a').
	self assert: (self testForm textfield2 text = 'a').
	self assert: (self testForm textfield3 text = 'a').
