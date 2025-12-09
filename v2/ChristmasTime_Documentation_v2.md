Here is your **updated `documentation.md`** with *full function + return support* added cleanly and consistently in ChristmasTime style.

---

# 🎄 ChristmasTime Scripting Language

### *Official Documentation — Version 0.2 (with Function Support)*

## 📚 Table of Contents

1. Introduction
2. Running ChristmasTime Scripts
3. Language Basics
4. Variables
5. Data Types
6. Printing (say)
7. Expressions
8. Conditions (if / else)
9. Loops (jingle … times)
10. Blocks
11. Comments
12. **Functions (hoho)**
13. Full Example Programs
14. Error Messages
15. Future Extensions (Roadmap)

---

## 🎁 1. Introduction

**ChristmasTime** is a lightweight, C-style scripting language designed for fun, simplicity, and extensibility.

```
gift count = 3
if count > 2 {
    say "Many gifts!"
}
```

---

## 🎄 2. Running ChristmasTime Scripts

### Run a file

```bash
./christmastime script.ct
```

### REPL (optional)

```bash
./christmastime
> say "Ho ho ho"
```

---

## ⭐ 3. Language Basics

ChristmasTime code consists of **statements** and **expressions**.

---

## 🎁 4. Variables (`gift`)

```
gift bags = 5
gift bags = bags + 1
```

---

## 🎨 5. Data Types

| Type    | Example     |
| ------- | ----------- |
| Number  | 12, 3.5     |
| String  | "Hello"     |
| Boolean | true, false |

---

## 🔔 6. Printing (`say`)

```
say "Merry Christmas"
say 1 + 2
say "Total: " + 5
```

---

## ➕ 7. Expressions

Supports arithmetic, comparison, parentheses, unary `!` and unary `-`.

---

## 🎅 8. Conditions

```
if count > 10 {
    say "Big sack!"
} else {
    say "Small sack."
}
```

---

## 🔁 9. Loops (`jingle ... times`)

```
jingle 3 times {
    say "Jingle bells!"
}
```

---

## 🌲 10. Blocks

```
{
    say "Inside!"
}
```

---

## ✏️ 11. Comments

```
# this is a comment
```

---

# 🎁 12. Functions (`hoho`) — NEW!

ChristmasTime supports defining and calling functions using the festive keyword **`hoho`**.

---

## 🎅 Function Definition

```
hoho greet(name) {
    say "Hello " + name
}
```

### Syntax

```
hoho <name>( <parameters> ) { <body> }
```

### Parameters

* comma-separated
* local to the function

---

## 🎁 Function Call

```
greet("Santa")
```

Arguments follow normal expression rules.

---

## 🎄 `return` Keyword

Functions can return a value:

```
hoho add(a, b) {
    return a + b
}
```

If a function reaches the end without a return statement, it implicitly returns `null`.

---

## 🌟 Example With Logic

```
hoho checkGifts(count) {
    if count > 10 {
        return "Lots of gifts!"
    }
    return "Not so many gifts."
}
```

---

## 🎅 Functions Returning Nothing

```
hoho laugh() {
    say "Ho ho ho!"
}
```

---

## 🏭 Scoping Rules

* Functions create a new **local scope**
* Variables inside a function do not leak
* Functions can read global variables but not write to them (unless explicitly designed to)

---

# 🎄 13. Full Example Programs

### Example 1 — Greeting Function

```
hoho greet(name) {
    say "Hello " + name
}

greet("Achim")
```

### Example 2 — Loops + Functions

```
hoho bells(n) {
    jingle n times {
        say "Jingle!"
    }
}

bells(3)
```

### Example 3 — Return Values

```
hoho add(a, b) {
    return a + b
}

gift result = add(5, 7)
say result
```

---

# ⚠️ 14. Errors

* Unknown variable
* Function not defined
* Duplicate parameter name
* Using `return` outside a function
* Missing braces
* Unexpected token

---

# 🚀 15. Future Extensions

* Arrays
* Objects
* Imports
* Bytecode VM
* Standard library
* Closures
* Native functions

---

If you'd like, I can also:

✅ Add syntax highlighting
✅ Create a downloadable `documentation.md` file
✅ Generate tests or examples
✅ Produce function grammar (EBNF)
✅ Extend the road map further

Just tell me!
