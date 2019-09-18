navigating
findByPosition: aPosition

	^self findByCriteria: [ :curMorph |
		self covers: curMorph position: aPosition]