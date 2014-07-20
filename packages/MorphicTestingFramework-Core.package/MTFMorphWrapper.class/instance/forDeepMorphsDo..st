private-accessing
forDeepMorphsDo: aBlock

	self morphs do: [ :curMorph |
		aBlock value: curMorph.
		self forDeepSubMorphsOf: curMorph do: aBlock.
	].