instance creation
newWith: Morphs withDelay: aNumber
	|wrapper|
	wrapper := self new.
	wrapper morphs: Morphs.
	wrapper delay: aNumber.
	^wrapper