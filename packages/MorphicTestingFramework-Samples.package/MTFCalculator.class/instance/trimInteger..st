private-utilities
trimInteger: aNumber

	(self isFraction: aNumber) ifTrue: [
		^aNumber asFloat asString.
	] ifFalse: [
		^aNumber asInteger asString.
	].