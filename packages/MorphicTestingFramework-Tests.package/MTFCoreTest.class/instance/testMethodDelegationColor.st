test cases
testMethodDelegationColor
	| testMorph wrapper |
	testMorph := Morph new.
	testMorph
		color: Color white;
		openInWorld.
		self assert: testMorph color = Color white.
	wrapper := MTFMorphWrapper newWith: {testMorph}.
	self assert: wrapper color = Color white.
	testMorph delete.