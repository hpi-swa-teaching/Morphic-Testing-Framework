test cases
testWrapperClickDelay

	| buttonWrapper timeStamp1 timeStamp2 |
	self delay: 1000. 
	timeStamp1 := DateAndTime current.
	buttonWrapper := MTFMorphWrapper newWith: {subject button}.
	buttonWrapper click.
	timeStamp2 := DateAndTime current.
	self assert: ((timeStamp2 - timeStamp1) asNanoSeconds > 1000000000).