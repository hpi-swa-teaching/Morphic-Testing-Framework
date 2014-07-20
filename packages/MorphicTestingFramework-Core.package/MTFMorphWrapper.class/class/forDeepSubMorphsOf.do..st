private-utilities
forDeepSubMorphsOf: aMorph do: aBlock
	
	aMorph submorphs do: [ :subMorph |
		aBlock value: subMorph.
		MTFMorphWrapper forDeepSubMorphsOf: subMorph do: aBlock.
	].