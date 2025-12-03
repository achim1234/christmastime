# 🎄 ChristmasTime — A Tiny Scripting Language in C

**ChristmasTime** is a fun, lightweight scripting language written in C, with a Christmas-themed syntax. Use `gift` for variables, `say` to print, and `jingle` for loops — perfect for learning interpreters or just having fun.

---

## 🚀 Features

- Simple variable system (`gift x = 5`)
- Print command (`say "Hello"`)
- Math and expressions
- Conditions (`if … { … }`)
- Loops (`jingle 3 times { … }`)
- Blocks and comments
- Fully implemented lexer, parser, AST evaluator
- Easy to extend C codebase

---

## 📦 Quick Start

### 1. Compile the interpreter
```bash
gcc christmastime.c -o christmastime -lm
```

### 2. Create a ChristmasTime script
Create a file called `project_test.ct`:
```ct
gift count = 3
jingle count times {
    say "Ho ho ho!"
}
```

### 3. Run the script
```bash
./christmastime project_test.ct
```

### 4. Or use the interactive console
```bash
./christmastime
```
Then type:
```ct
say "HoHoHo"
```
Press enter to see the output instantly.

---

## 🎅 Example Program
```ct
gift gifts = 5
if gifts > 3 {
    say "Many gifts!"
} else {
    say "Just a few gifts."
}
```

---

## 🚀 Roadmap
- Functions support
- Arrays and objects
- Import modules
- Bytecode VM for faster execution
- Standard library (random, string functions, file I/O)
- Enhanced error handling

---

## 📄 License
This project is open source and available under the MIT License.

