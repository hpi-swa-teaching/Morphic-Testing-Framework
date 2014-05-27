assertion
contains: aMorph

	^ (self morphs select: [:morph | morph = aMorph]) isEmpty not.