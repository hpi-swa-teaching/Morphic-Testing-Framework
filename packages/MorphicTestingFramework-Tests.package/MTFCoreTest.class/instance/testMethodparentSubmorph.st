test cases
testMethodparentSubmorph
	| parentMorph subMorph subWrapper |
	parentMorph := Morph new.
	parentMorph openInWorld.
	subMorph := Morph new.
	parentMorph addMorph: subMorph.
	subWrapper := MTFMorphWrapper newWith: {subMorph}.
	self assert: subWrapper parent = parentMorph.
	parentMorph delete.
	
	