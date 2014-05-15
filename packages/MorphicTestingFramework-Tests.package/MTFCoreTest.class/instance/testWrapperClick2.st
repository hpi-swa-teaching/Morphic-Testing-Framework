as yet unclassified
testWrapperClick2
	| button buttonWrapper |

	button := SimpleButtonMorph new.
	button
		target: self;
		actionSelector: #click.
	button openInWorld.
	
	buttonWrapper := MTFMorphWrapper newWith: (button).
	buttonWrapper click.
	
	self assert: (button color = Color blue).
	button delete