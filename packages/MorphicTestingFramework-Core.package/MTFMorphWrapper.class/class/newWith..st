instance creation
newWith: Morphs
	|wrapper|
	wrapper := self new.
	wrapper morphs: Morphs.
	^wrapper