finding
findByLabel: aLabel

	^MTFMorphWrapper newWith: (self findByCriteria: [ :morph |
		(self labelSupportFor: morph) and: [morph label = aLabel].
	]) withDelay: self delay.