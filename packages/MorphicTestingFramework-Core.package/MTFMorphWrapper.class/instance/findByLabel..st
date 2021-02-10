navigating
findByLabel: aLabel

	^self findByCriteria: [ :morph |
		(morph respondsTo: #label) and: [morph label = aLabel]]