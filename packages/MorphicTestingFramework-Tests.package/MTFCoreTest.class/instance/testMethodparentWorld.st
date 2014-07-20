test cases
testMethodParentWorld

	| testMorph wrapper |
	testMorph := Morph new.
	testMorph openInWorld.
	wrapper := MTFMorphWrapper newWith: {testMorph}.
	self assert: wrapper parent = testMorph owner.
	testMorph delete.