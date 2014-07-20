private-accessing
forDeepSubMorphsOf: aMorph do: aBlock
	
	aMorph submorphs do: [ :subMorph |
		aBlock value: subMorph.
		self forDeepSubMorphsOf: subMorph do: aBlock.
	].