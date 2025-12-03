# 🎄 ChristmasTime Scripting Language

### *Official Documentation --- Version 0.1*

## 📚 Table of Contents

1.  Introduction\
2.  Running ChristmasTime Scripts\
3.  Language Basics\
4.  Variables\
5.  Data Types\
6.  Printing (say)\
7.  Expressions\
8.  Conditions (if / else)\
9.  Loops (jingle ... times)\
10. Blocks\
11. Comments\
12. Full Example Programs\
13. Error Messages\
14. Future Extensions (Roadmap)

------------------------------------------------------------------------

## 🎁 1. Introduction

**ChristmasTime** is a lightweight, C-based scripting language designed
for fun, simplicity, and extendability.

    gift count = 3
    if count > 2 {
        say "Many gifts!"
    }

------------------------------------------------------------------------

## 🎄 2. Running ChristmasTime Scripts

### Running a file

``` bash
./christmastime script.ct
```

### REPL (optional)

``` bash
./christmastime
> say "Ho ho ho"
```

------------------------------------------------------------------------

## ⭐ 3. Language Basics

ChristmasTime code consists of **statements** and **expressions**.

------------------------------------------------------------------------

## 🎁 4. Variables (gift)

    gift bags = 5
    gift bags = bags + 1

------------------------------------------------------------------------

## 🎨 5. Data Types

  Type      Example
  --------- -------------
  Number    12, 3.5
  String    "Hello"
  Boolean   true, false

------------------------------------------------------------------------

## 🔔 6. Printing (say)

    say "Merry Christmas"
    say 1 + 2
    say "Total: " + 5

------------------------------------------------------------------------

## ➕ 7. Expressions

Supports arithmetic, comparison, parentheses, and unary `!` and `-`.

------------------------------------------------------------------------

## 🎅 8. Conditions (if / else)

    if count > 10 {
        say "Big sack!"
    } else {
        say "Small sack."
    }

------------------------------------------------------------------------

## 🔁 9. Loops (jingle ... times)

    jingle 3 times {
        say "Jingle bells!"
    }

------------------------------------------------------------------------

## 🌲 10. Blocks

    {
        say "Inside!"
    }

------------------------------------------------------------------------

## ✏️ 11. Comments

    # this is a comment

------------------------------------------------------------------------

## 🎄 12. Full Example Programs

### Example 1

    say "Welcome!"
    gift gifts = 5
    say "Gifts: " + gifts

### Example 2

    gift temperature = -3
    if temperature < 0 {
        say "Snow outside!"
    }

### Example 3

    jingle 5 times {
        say "Ho ho ho!"
    }

------------------------------------------------------------------------

## ⚠️ 13. Errors

-   Unknown variable\
-   Missing braces\
-   Unexpected tokens

------------------------------------------------------------------------

## 🚀 14. Future Extensions

-   Functions\
-   Arrays\
-   Objects\
-   Imports\
-   Bytecode VM\
-   Standard library
