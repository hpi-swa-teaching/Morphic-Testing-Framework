as yet unclassified
findByClass: aClass

	| foundMorph wrapper |
	foundMorph := OrderedCollection new.
	self morphs do: [:morph| foundMorph add: (morph submorphOfClass: aClass)].
	wrapper := MTFMorphWrapper newWith: foundMorph.
	^ wrapper.