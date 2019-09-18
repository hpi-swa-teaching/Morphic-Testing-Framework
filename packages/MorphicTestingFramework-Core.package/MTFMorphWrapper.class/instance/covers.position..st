assertion
covers: aMorph position: aPosition

	^(aMorph position x < aPosition x) and: 
	[aMorph position y < aPosition y] and: 
	[aMorph position x + aMorph width > aPosition x] and: 
	[aMorph position y + aMorph height > aPosition y]