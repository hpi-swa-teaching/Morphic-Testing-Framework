private-finding
forDeepMorphsDo: aBlock

	self morphs do: [ :curMorph |
		aBlock value: curMorph.
		self forDeepSubMorphsOf: curMorph do: aBlock.
	].