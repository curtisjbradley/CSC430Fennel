## About

This is a project for CSC430. We created and updated a programming languaguge named QWJZ throughout the quarter. The purpose of this project is to recreate QWJZ5 in Fennel.

## How to Run

Make sure to have both Lua and Fennel installed. You can go [here](https://fennel-lang.org/setup). If you have homebrew, you can just run:

```
brew install fennel
brew install lua
```

Afterwards, when you want to run the code in assignment.fnl, run:

```
lua -e "require('fennel').dofile('assignment.fnl')"
```

## Running in UNIX

Install instructions [here](https://apt.technomancy.us/).

Install instructions with other package managers can be found [here](https://wiki.fennel-lang.org/Packaging).

Afterwards you can run the program by
```bash
fennel assignment.fnl
```

## Editing code
There isn't a designated IDE to use that works with fennel, however I found that using Sublime works well if the correct package is installed.
```
Launch Sublime
Control + Shift + P to open Package Manager.
Search for and install Fennel
Restart sublime
(If that doesn't work Control Shift P and run 'Set Syntax: Fennel')
