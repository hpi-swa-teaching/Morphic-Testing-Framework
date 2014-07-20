private-event-generation
generateMouseEvent: aSymbol button: aMouseButton on: aMorph
	
	^ self generateMouseEvent: aSymbol
		button: aMouseButton
		modifiers: MTFMorphWrapper emptyEventModifier
		on: aMorph