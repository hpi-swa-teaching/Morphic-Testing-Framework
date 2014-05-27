finding
findByName: aName

	^MTFMorphWrapper newWith: (self findByCriteria: [ :curMorph |
		curMorph nameOfObjectRepresented = aName.
	])