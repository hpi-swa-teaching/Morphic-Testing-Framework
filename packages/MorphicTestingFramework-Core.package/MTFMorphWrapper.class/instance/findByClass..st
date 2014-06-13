finding
findByClass: aClass

	^MTFMorphWrapper newWith: (self findByCriteria: [ :curMorph |
		curMorph class = aClass.
	]) withDelay: self delay.