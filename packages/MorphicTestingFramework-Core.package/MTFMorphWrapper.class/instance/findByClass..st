navigating
findByClass: aClass

	^self findByCriteria: [ :curMorph |
		curMorph class = aClass.
	]