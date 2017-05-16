# Morphic Testing Framework

[![Build Status](https://travis-ci.org/HPI-SWA-Teaching/Morphic-Testing-Framework.svg?branch=master)](https://travis-ci.org/HPI-SWA-Teaching/Morphic-Testing-Framework)

### Morphic Testing Framework

- [Introduction](#Introduction)
- [Getting started](#Getting started)
  - [Installation](#Installation)
  - [First test](#First test)
  - [Give me more](#Give me more)
- [API](#API)
  - [Setup](#Setup)
  - [Simulate interactions](#Simulate interactions)
  - [Find morphs](#Find morphs)

# Introduction

Automated UI testing should be an essential part of your application as it gets bigger. While browser applications rely on the popular Selenium WebDriver it was not possible to efficiently test Morphic user interfaces - until now.

# Getting started

## Installation

### Unix

Curl must be installed on your operating system.

``` Smalltalk
Metacello new
    baseline: 'Project12';
    repository: 'github://HPI-SWA-Teaching/Morphic-Testing-Framework:master/packages';
    load.
```

### Windows (and also Unix)

Unfortunately, the simple approach does not work under Windows, because github-protocol is not supported yet. In order to install on Windows, execute the following command inside your `Contents\Resources` directory:

```
git clone https://github.com/HPI-SWA-Teaching/Morphic-Testing-Framework.git morphic-testing-framework
```

Then, execute this snippet in a workspace of the corresponding Squeak image:

```Smalltalk
Metacello new
    baseline: 'Project12';
    filetreeDirectory: (MCFileTreeFileUtils current default / 'morphic-testing-framework' / 'packages') fullName;
    load.
```

##First test

You can write your UI tests just as your unit tests and run them in the SUnit test runner. To create a UI test case, simply inherit it from MTFTestCase and you get all the SUnit stuff plus the frameworks UI testing abilities:
```Smalltalk
MTFTestCase subclass: #MyUITestCase
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyCategory'
```

To test your interface, overwrite the setUp message:

```Smalltalk
MyUITestCase>>setUp
    self wantsToTest: MyUserInterface new.
```

Now we could test that our button named 'fancy' changes its color when it is clicked:
```Smalltalk
MyUITestCase>>testFancyClick
    |button|
    button := self subject findByClass: SimpleButtonMorph.
    self assert: button color = Color white.
    button click.
    self assert: button color = Color red.
```

##Give me more
For more detailed examples of how to use the framework, refer to the API section or take a look at the MTFCalculatorTests in the Morphic-Testing-Framework-Tests package.

# API

## Setup
##### MTFTestCase>>wantsToTest: aMorph
`MTFTestCase>>wantsToTest: aMorph` should be called in your `MTFTestCase>>setUp`.
It sets the subject of the test case to be accessible as an MTFMorphWrapper in your test methods.

Example:
```Smalltalk
MTFTestCase>>setUp
    self wantsToTest: MyUserInterface new.
```

##### MTFTestCase>>subject
Returns the morph wrapper for the morph specified in `MTFTestCase>>wantsToTest: aMorph`.

##### MTFTestCase>>slowTestBy: aNumber
By default all tests run headless meaning you cannot trace the performed interactiosn visually. To change this, this message allows you to delay all interactions by the given number of milliseconds. You can call `MTFTestCase>>slowTestBy:` to run the test visually. In this case the subject is shown in the world and the interactions are slowed down. Therefore, you can follow the test. Note: You should not call openInWorld on the morph you want to test by yourself.

## Simulate interactions
#### Mouse Clicks
- MTFMorphWrapper>>leftMouseClickWith
- MTFMorphWrapper>>leftMouseClickWith: modifiers   
- MTFMorphWrapper>>rightMouseClickWith
- MTFMorphWrapper>>rightMouseClickWith: modifiers
- MTFMorphWrapper>>middleMouseClickWith
- MTFMorphWrapper>>middleMouseClickWith: modifiers

Sends a left/right/middle click event to all morphs contained in the wrapper. The `modifiers` argument is used to simulate clicks combined with pressed SHIFT, CTRL or CMD and can be omitted. If you want to use modifiers use the conctant defined in the MTFMorphWrapper as in the following example:
```
MyTestCase>>testThis
    self subject leftMouseClickWith: MTFMorphWrapper shiftModifier.
    self subject rightMouseClick.
```
#### Mouse Down

- MTFMorphWrapper>> leftMouseDownWith
- MTFMorphWrapper>> leftMouseDownWith: modifiers 
- MTFMorphWrapper>> rightMouseDownWith
- MTFMorphWrapper>> rightMouseDownWith: modifiers
- MTFMorphWrapper>> middleMouseDownWith
- MTFMorphWrapper>> middleMouseDownWith: modifiers  

Sends a left/right/middle mouse down event to all morphs contained in the wrapper. The `modifiers` argument is used to simulate mouse down events combined with pressed SHIFT, CTRL or CMD and can be omitted. If you want to use modifiers use the conctant defined in the MTFMorphWrapper as in the following example:
```Smalltalk
MyTestCase>>testThis
    self subject leftMouseDownWith: MTFMorphWrapper shiftModifier.
    self subject rightMouseDown.
```

#### Mouse Up
- MTFMorphWrapper>> leftMouseUpWith
- MTFMorphWrapper>> leftMouseUpWith: modifiers
- MTFMorphWrapper>> rightMouseUpWith    
- MTFMorphWrapper>> rightMouseUpWith: modifiers
- MTFMorphWrapper>> middleMouseUpWith
- MTFMorphWrapper>> middleMouseUpWith: modifiers

Sends a left/right/middle mouse up event to all morphs contained in the wrapper. The `modifiers` argument is used to simulate mouse downup events combined with pressed SHIFT, CTRL or CMD and can be omitted. If you want to use modifiers use the conctant defined in the MTFMorphWrapper as in the following example:
```Smalltalk
MyTestCase>>testThis
    self subject leftMouseUpWith: MTFMorphWrapper shiftModifier.
    self subject rightMouseUp.
```
#### Key Events
##### MTFMorphWrapper>>sendKey: aCharacter
Sends a keystroke event for the specified character to all morphs contained in the wrapper.

##### MTFMorphWrapper>>sendKeys: aString
Sends a the specified string to all morphs contained in the wrapper.
Internally, `sendKey ` is used, so
```Smalltalk
MyTestCase>>testThis
    self subject sendKey: 'H'.
    self subject sendKey: 'e'.
    self subject sendKey: 'l'.
    self subject sendKey: 'l'.
    self subject sendKey: 'o'.
```
is equivalent to 
```Smalltalk
MyTestCase>>testThis
    self subject sendKeys: 'Hello'.
```
##### MTFMorphWrapper>>sendKeyEvent: aType characterValue: aValue
This is the generic method you can use to create custom key. Here is an example how this is used to implement `MTFMorphWrapper>>sendKey:`. 
```Smalltalk
MTFMorphWrapper>>sendKey: aCharacter
    self sendKeyEvent: #keystroke characterValue: aCharacter asInteger.
```

## Find morphs

##### MTFMorphWrapper>>findByName: aString
Recursively finds all submorphs that have the passed name and returns a wrapper containing them.

Note: Giving mutliple morphs the same name is generally a bad practice.

##### MTFMorphWrapper>>findByClass: aClass
Recursively finds all submorphs that are instances of the passed class and returns a wrapper containing them.

##### MTFMorphWrapper>>findByLabel: aString
Recursively finds all submorphs that have the passed label and returns a wrapper containing them.

Note: This refers to the label message available for example on a SimpleButtonMorph. This only works on the Morph classes explicitly defined in the framework. can
