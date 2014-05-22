as yet unclassified
findByName: aName

	| foundMorph wrapper |
	foundMorph := self morph submorphNamed: aName.
	wrapper := MTFMorphWrapper newWith: foundMorph.
	^ wrapper.