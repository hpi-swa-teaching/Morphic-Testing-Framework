test cases
testWrapperNewWithDelay

	| buttonWrapper timeStamp1 timeStamp2 |
	buttonWrapper := MTFMorphWrapper newWith: {subject button} withDelay: 1000.
	timeStamp1 := DateAndTime current.
	buttonWrapper click.
	timeStamp2 := DateAndTime current.
	self assert: ((timeStamp2 - timeStamp1) asNanoSeconds >= 1000000000).