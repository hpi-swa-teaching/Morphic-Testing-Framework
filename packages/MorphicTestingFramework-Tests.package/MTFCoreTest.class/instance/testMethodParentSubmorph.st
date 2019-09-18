test cases
testMethodParentSubmorph

	| parentMorph subMorph subWrapper |
	parentMorph := Morph new.
	subMorph := Morph new.
	subWrapper := MTFMorphWrapper newWith: {subMorph}.
	parentMorph 
		openInWorld;
		addMorph: subMorph.
	self assert: subWrapper parent = parentMorph.
	parentMorph delete.