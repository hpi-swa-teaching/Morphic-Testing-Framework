navigating
findByLabel: aLabel

	^self findByCriteria: [ :morph |
		(MTFMorphWrapper doesSupportLabel: morph) and: [morph label = aLabel]]