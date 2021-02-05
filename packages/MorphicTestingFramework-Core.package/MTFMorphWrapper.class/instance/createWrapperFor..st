navigating
createWrapperFor: morphs

	^(MTFMorphWrapper newWith: morphs withDelay: self delay)
		hand: self hand;
		yourself