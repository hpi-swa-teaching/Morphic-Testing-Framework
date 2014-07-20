private-utilities
is: aClass subclassOfAny: anArray
	
	anArray detect: [:element | (aClass = element) or: [aClass inheritsFrom: element]] 
		ifFound: [ :foundParentClass | ^true] 
		ifNone: [^false].