test cases
testWrapperDelayMorphInWorld

	| timeStamp1 timeStamp2 |
	self slowTestBy: 1000.
	timeStamp1 := DateAndTime current.
	subject click.
	timeStamp2 := DateAndTime current.
	self assert: ((timeStamp2 - timeStamp1) asNanoSeconds >= 1000000000).