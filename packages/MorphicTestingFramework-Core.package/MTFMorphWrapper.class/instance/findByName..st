navigating
findByName: aName

	^self findByCriteria: [ :curMorph |
		curMorph nameOfObjectRepresented = aName.
	]