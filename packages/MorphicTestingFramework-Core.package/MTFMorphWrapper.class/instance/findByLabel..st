finding
findByLabel: aLabel

	^MTFMorphWrapper newWith: (self findByCriteria: [ :labeledMorph |
		labeledMorph label = aLabel.
	])