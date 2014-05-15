as yet unclassified
testWrapperClick
	| form buttonWrapper |

	form := SimpleForm new.
	form openInWorld.
	self assert: (form color = Color white).
	
	buttonWrapper := MTFMorphWrapper newWith: (form button).
	buttonWrapper click.
	
	self assert: (form color = Color blue).
	form delete