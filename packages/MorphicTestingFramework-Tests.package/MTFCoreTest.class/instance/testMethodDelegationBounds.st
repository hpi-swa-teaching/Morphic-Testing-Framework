test cases
testMethodDelegationBounds
	| testMorph wrapper specificBounds |
	specificBounds := 10@10 corner: 100@100.
	testMorph := Morph new.
	testMorph 
		openInWorld;
		bounds: specificBounds.
	wrapper := MTFMorphWrapper newWith: testMorph.
	self assert: wrapper bounds = specificBounds.
	testMorph delete.