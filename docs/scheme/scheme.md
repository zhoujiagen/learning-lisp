# Scheme

- Scheme (programming language): https://en.wikipedia.org/wiki/Scheme_(programming_language)
- Standards: https://schemers.org/Documents/Standards/
- MIT/GNU Scheme: https://groups.csail.mit.edu/mac/projects/scheme/
- 文件扩展名: [What's the proper scheme file extension?](https://stackoverflow.com/questions/36240629/whats-the-proper-scheme-file-extension)

## MIT-GNU Scheme

```
"D:\software\MIT-GNU Scheme\bin\mit-scheme.exe" \
  --library "D:\software\MIT-GNU Scheme\lib" \
  --heap 1024

"D:\software\MIT-GNU Scheme\bin\mit-scheme.exe" \
  --library "D:\software\MIT-GNU Scheme\lib" \
  --heap 1024 \
  --edit

"D:\software\MIT-GNU Scheme\bin\mit-scheme.exe" \
  --library "D:\software\MIT-GNU Scheme\lib" \
  --heap 1024 \
  --quiet \
  --load main.scm

```

VS Code:

```
    "code-runner.executorMapByFileExtension": {
        ".scm": "mit-scheme.exe --library \"D:\\software\\MIT-GNU Scheme\\lib\" --heap 1024",

    "code-runner.executorMap": {
        "scheme": "mit-scheme.exe --library \"D:\\software\\MIT-GNU Scheme\\lib\" --heap 1024 --quiet --load",
```


## Chez Scheme

https://github.com/cisco/chezscheme

## R6RS

```
Introduction................................... 3

Description of the language
1 Overview of Scheme........................... 5
1.1 Basic types................................ 5
1.2 Expressions................................ 6
1.3 Variables and binding...................... 6
1.4 Definitions................................ 6
1.5 Forms...................................... 7
1.6 Procedures................................. 7
1.7 Procedure calls and syntactic keywords..... 7
1.8 Assignment................................. 7
1.9 Derived forms and macros................... 8
1.10 Syntactic data and datum values........... 8
1.11 Continuations............................. 8
1.12 Libraries................................. 9
1.13 Top-level programs........................ 9
2 Requirement levels........................... 9
3 Numbers...................................... 10
3.1 Numerical tower............................ 10
3.2 Exactness.................................. 10
3.3 Fixnums and flonums........................ 10
3.4 Implementation requirements................ 10
3.5 Infinities and NaNs........................ 11
3.6 Distinguished -0.0......................... 11
4 Lexical syntax and datum syntax.............. 11
4.1 Notation................................... 12
4.2 Lexical syntax............................. 12
4.3 Datum syntax............................... 16
5 Semantic concepts............................ 17
5.1 Programs and libraries..................... 17
5.2 Variables, keywords, and regions........... 17
5.3 Exceptional situations..................... 18
5.4 Argument checking.......................... 18
5.5 Syntax violations.......................... 19
5.6 Safety..................................... 19
5.7 Boolean values............................. 19
5.8 Multiple return values..................... 19
5.9 Unspecified behavior....................... 20
5.10 Storage model............................. 20
5.11 Proper tail recursion..................... 20
5.12 Dynamic extent and the dynamic environment 20
6 Entry format................................. 20
6.1 Syntax entries............................. 21
6.2 Procedure entries.......................... 21
6.3 Implementation responsibilities............ 22
6.4 Other kinds of entries..................... 22
6.5 Equivalent entries......................... 22
6.6 Evaluation examples........................ 22
6.7 Naming conventions......................... 23
7 Libraries.................................... 23
7.1 Library form............................... 23
7.2 Import and export levels................... 25
7.3 Examples................................... 26
8 Top-level programs........................... 27
8.1 Top-level program syntax................... 27
8.2 Top-level program semantics................ 28
9 Primitive syntax............................. 28
9.1 Primitive expression types................. 28
9.2 Macros..................................... 29
10 Expansion process........................... 29
11 Base library................................ 31
11.1 Base types................................ 31
11.2 Definitions............................... 31
11.3 Bodies.................................... 32
11.4 Expressions............................... 32
11.5 Equivalence predicates.................... 37
11.6 Procedure predicate....................... 39
11.7 Arithmetic................................ 39
11.8 Booleans.................................. 47
11.9 Pairs and lists........................... 47
11.10Symbols................................... 49
11.11Characters................................ 50
11.12Strings................................... 50
11.13Vectors................................... 51
11.14Errors and violations..................... 52
11.15Control features.......................... 53
11.16Iteration................................. 55
11.17Quasiquotation............................ 55
11.18Binding constructs for syntactic keywords. 56
11.19Macro transformers........................ 57
11.20Tail calls and tail contexts.............. 59

Appendices
A Formal semantics............................. 61
A.1 Background................................. 61
A.2 Grammar.................................... 62
A.3 Quote...................................... 65
A.4 Multiple values............................ 65
A.5 Exceptions................................. 67
A.6 Arithmetic and basic forms................. 69
A.7 Lists...................................... 69
A.8 Eqv........................................ 69
A.9 Procedures and application................. 70
A.10 Call/cc and dynamic wind.................. 72
A.11 Letrec.................................... 73
A.12 Underspecification........................ 74
B Sample definitions for derived forms......... 75
C Additional material.......................... 77
D Example...................................... 77
E Language changes............................. 79
References..................................... 80
Alphabetic index of definitions of concepts,
keywords, and procedures....................... 82
```
