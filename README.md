#SWTI2014-Project-12

[![Build Status](https://travis-ci.org/SWTI2014/SWTI2014-Project-12.svg?branch=master)](https://travis-ci.org/SWTI2014/SWTI2014-Project-12)

MorphicTestingFrameworks extends your morphic UI with great testing capabilities. Give it a try!

- [Introduction](#Introduction)
- [Getting started](#Getting started)
  - [Installation](#Installation)
    - [Unix](#Unix)
    - [Windows and Unix](#Windows and Unix)
  - [First test](#First test)
  - [Give me more](#Give me more)
- [API](#API)
  - [Setup](#Setup)
  - [Simulate interactions](#Simulate interactions)
  - [Finding morphs](#Finding morphs)


<a name="Introduction"></a>
#Introduction

Automated UI testing should be an essential part of your application as it gets bigger. While browser applications rely on the popular Selenium WebDriver it was not possible to efficiently test Morphic user interfaces - unituntil now.

<a name="Getting started"></a>
#Getting started

<a name="Installation"></a>
##Installation

<a name="Unix"></a>
###Unix

Curl must be installed on your operating system.

``` 
Metacello new
  baseline: 'Project12';
  repository: 'github://SWTI2014/SWTI2014-Project-12:master/packages';
  load.
```

<a name="Windows and Unix"></a>
###Windows and Unix

Unfortunately, the simple approach does not work under Windows, because github-protocol is not supported yet. In order to install on Windows, execute the following command inside your `Contents\Resources` directory:

```
git clone https://github.com/SWTI2014/SWTI2014-Project-12.git morphic-testing-framework
```

Then, execute this snippet in a workspace of the corresponding Squeak image:

```
Metacello new
  baseline: 'Project12';
  filetreeDirectory: (MCFileTreeFileUtils current default / 'morphic-testing-framework' / 'packages') fullName;
  load.
```

<a name="First test"></a>
##First test

You can write your UI tests just as your unit tests and run them in the SUnit test runner. To create a UI test case, simply inherit it from MTFTestCase and you get all the SUnit stuff plus the frameworks UI testing abilities:
```
MTFTestCase subclass: #MyUITestCase
    instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MyCategory'
```

To test your interface, overwrite the setUp message:

```
MyUITestCase>>setUp
    self wantsToTest: MyUserInterface new.
```

Now we could test that our button named 'fancy' changes its color when it is clicked:
```
MyUITestCase>>testFancyClick
    
    |button|
    button := self subject findByClass: SimpleButtonMorph.
    self assert: button color = Color white.
    button click.
    self assert: button color = Color red.
```

<a name="Give me more"></a>
##Give me more
For more detailed examples of how to use the framework, refer to the API section or take a look at the MTFCalculatorTests in the Morphic-Testing-Framework-Tests package.

<a name="API"></a>
#API

<a name="Setup"></a>
##Setup

####MTFTestCase>>wantsToTest: aMorph
Should be called in your `MTFTestCase>>setUp`.
Sets the subject of the test case to be accessible as an MTFMorphWrapper in your test methods.
In case the interactions are "slowed down" (see MTFTestCase>>slowTestBy) to run the tests visually the morph is automatically opened in the world.
Note: You should not call openInWorld on the morph you want to test by yourself.

Example:
```
MTFTestCase>>setUp
    self wantsToTest: MyUserInterface new.
```

####MTFTestCase>>subject
Returns the morph wrapper for the morph specified in `MTFTestCase>>wantsToTest: aMorph`.

####MTFTestCase>>slowTestBy: aNumber
By default all tests run headless meaning you cannot trace the performed interactiosn visually. To change this, this message allows you to delay all interactions by the given number of milliseconds.

<a name="Simulate interactions"></a>
##Simulate interactions

####MTFMorphWrapper>>click
Sends a click event to all morphs contained in the wrapper.

####MTFMorphWrapper>>sendKey: aCharacter
Sends a keystroke event for the specified character to all morphs contained in the wrapper.

####MTFMorphWrapper>>sendKeys: aString
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

<a name="Finding morphs"></a>
##Finding morphs

####MTFMorphWrapper>>findByName: aString
Recursively finds all submorphs that have the passed name and returns a wrapper containing them.

Note: Giving mutliple morphs the same name is generally a bad practice.

####MTFMorphWrapper>>findByClass: aClass
Recursively finds all submorphs that are instances of the passed class and returns a wrapper containing them.

####MTFMorphWrapper>>findByLabel: aString
Recursively finds all submorphs that have the passed label and returns a wrapper containing them.

Note: This refers to the label message available for example on a SimpleButtonMorph. This only works on the Morph classes explicitly defined in the framework.
