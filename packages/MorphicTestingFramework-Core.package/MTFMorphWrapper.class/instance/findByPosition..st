navigating
findByPosition: aPosition

	^self findByCriteria: [ :curMorph |
		(curMorph position x < aPosition x) and: (curMorph position y < aPosition y) and: (curMorph position x + curMorph width > aPosition x) and: (curMorph position y + curMorph height > aPosition y).
	]