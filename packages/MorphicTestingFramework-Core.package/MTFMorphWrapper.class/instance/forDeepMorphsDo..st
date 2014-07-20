private-accessing
forDeepMorphsDo: aBlock

	self morphs do: [ :curMorph |
		aBlock value: curMorph.
		MTFMorphWrapper forDeepSubMorphsOf: curMorph do: aBlock.
	].