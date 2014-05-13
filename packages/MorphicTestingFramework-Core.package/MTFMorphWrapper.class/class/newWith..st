instance creation
newWith: aMorph
	|wrapper|
	wrapper := self new.
	wrapper morph: aMorph.
	^wrapper