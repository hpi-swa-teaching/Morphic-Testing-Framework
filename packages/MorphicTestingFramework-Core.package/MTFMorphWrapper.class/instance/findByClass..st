as yet unclassified
findByClass: aClass

	| foundMorph wrapper |
	foundMorph := self morph submorphOfClass: aClass.
	wrapper := MTFMorphWrapper newWith: foundMorph.
	^ wrapper.