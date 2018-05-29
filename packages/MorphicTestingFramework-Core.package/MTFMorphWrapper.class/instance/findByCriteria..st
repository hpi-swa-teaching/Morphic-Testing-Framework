navigating
findByCriteria: aBlock

	| foundMorphs |
	foundMorphs := Set new.
	self forDeepMorphsDo: [ :curMorph |
		(aBlock value: curMorph) ifTrue: [foundMorphs add: curMorph]].
	^self createWrapperFor: foundMorphs