SWTI2014-Project-12
===================
[![Build Status](https://travis-ci.org/SWTI2014/SWTI2014-Project-12.svg?branch=master)](https://travis-ci.org/SWTI2014/SWTI2014-Project-12)


----------

Documentation
=============

If you want to test a Morph, then you should use the Morphic Testing Framework. It comes with a MTFTestCase and a MTFMorphWrapper that will make interaction with bigger morphic user interfaces a lot easier.

To get a little familiar with the interface, we added a simple calculator morph with some tests.

Getting started
---------------

Create a Test-class to your application. For our MTFCalculator, we will create MTFCalculatorTest and implement the basic setUp and tearDown methods. This is a nice way to write short understandable tests:

    setUp
	
	    self wantsToTest: MTFCalculator new.

Within the setUp we can specify the Morph/Application we want to test. The MTFTestCase will then provide the morph in every test as *subject*.

	tearDown 

	    self deleteMorphs.

*deleteMorphs* will close the application and also all other morphs or wrapper.
