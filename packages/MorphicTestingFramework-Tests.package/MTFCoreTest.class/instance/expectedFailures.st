running
expectedFailures 

	^ (#('Squeak4.6' 'Squeak5.0') includes: Smalltalk version)
		ifTrue: [#(testWrapperClickCtrl)]
		ifFalse: [#(testWrapperClickCtrlNoOp)]