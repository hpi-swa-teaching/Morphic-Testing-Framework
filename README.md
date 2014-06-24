SWTI2014-Project-12
===================
[![Build Status](https://travis-ci.org/SWTI2014/SWTI2014-Project-12.svg?branch=master)](https://travis-ci.org/SWTI2014/SWTI2014-Project-12)


----------

#Introduction

Automated UI testing should be an essential part of your application as it gets bigger. While browser applications rely on the popular Selenium WebDriver it was not possible to efficiently test Morphic user interfaces - unituntil now.

#Getting started

##Installation



##First test

Create a Test-class to your application. For our MTFCalculator, we will create MTFCalculatorTest and implement the basic setUp and tearDown methods. This is a nice way to write short understandable tests:

    setUp
    
	    self wantsToTest: MTFCalculator new.

Within the setUp we can specify the Morph/Application we want to test. The MTFTestCase will then provide the morph in every test as *subject*.

	tearDown 

	    self deleteMorphs.

*deleteMorphs* will close the application and also all other morphs or wrapper.

#API

##Setup

###MTFTestCase

####wantsToTest: aMorph
Should be called in your `MTFTestCase>>setUp`.
Sets the subject of the test case to be accessible as an MTFMorphWrapper in your test methods.
In case the interactions are "slowed down" (see MTFTestCase>>slowTestBy) to run the tests visually the morph is automatically opened in the world.
Note: You should not call openInWorld on the morph you want to test by yourself.

Example:
```
    MTFTestCase>>setUp
        self wantsToTest: MyUserInterface new.
```

####subject
Returns the morph wrapper for the morph specified in `MTFTestCase>>wantsToTest: aMorph`.

####slowTestBy: aNumber
By default all tests run headless meaning you cannot trace the performed interactiosn visually. To change this, this message allows you to delay all interactions by the given number of milliseconds.

###MTFMorphWrapper

####click
Sends a click event to all morphs contained in the wrapper.

####sendKey: aCharacter
Sends a keystroke event for the specified character to all morphs contained in the wrapper.

####sendKeys: aString
Sends a the specified string to all morphs contained in the wrapper.
Internally, `sendKey ` is used, so
```
    MyTestCase>>testThis
        self subject sendKey: 'H'.
        self subject sendKey: 'e'.
        self subject sendKey: 'l'.
        self subject sendKey: 'l'.
        self subject sendKey: 'o'.
```
is equivalent to 
```
    MyTestCase>>testThis
        self subject sendKeys: 'Hello'.
```

##Finding morphs

##Simulate interactions
