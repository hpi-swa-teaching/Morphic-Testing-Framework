as yet unclassified
findByName: aName

	| foundMorph wrapper |
	foundMorph := OrderedCollection new.
	self morphs do: [:morph | foundMorph add: (morph submorphNamed: aName)].
	wrapper := MTFMorphWrapper newWith: foundMorph.
	^ wrapper.