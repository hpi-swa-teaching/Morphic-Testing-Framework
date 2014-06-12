private-utilities
generateNumberButtonMorphFor: aNumber

	^(self generateButtonMorphLabeled: aNumber asString)
		target: self;
		actionSelector: ('handleClickNumber' , aNumber asString) asSymbol