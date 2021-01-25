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

## R5RS

```
Introduction 	                                    2
1 Overview of Scheme 	                            3
1.1 Semantics 	                                  3
1.2 Syntax 	                                      3
1.3 Notation and terminology 	                    3
2 Lexical conventions 	                          5
2.1 Identifiers                                   5
2.2 Whitespace and comments 	                    5
2.3 Other notations 	                            5
3 Basic concepts 	                                6
3.1 Variables, syntactic keywords, and regions 	  6
3.2 Disjointness of types 	                      6
3.3 External representations 	                    6
3.4 Storage model 	                              7
3.5 Proper tail recursion 	                      7
4 Expressions 	                                  8
4.1 Primitive expression types 	                  8
4.2 Derived expression types 	                    10
4.3 Macros 	                                      13
5 Program structure 	                            16
5.1 Programs 	                                    16
5.2 Definitions 	                                16
5.3 Syntax definitions 	                          17
6 Standard procedures 	                          17
6.1 Equivalence predicates 	                      17
6.2 Numbers 	                                    19
6.3 Other data types 	                            25
6.4 Control features 	                            31
6.5 Eval 	                                        35
6.6 Input and output 	                            35
7 Formal syntax and semantics 	                  38
7.1 Formal syntax 	                              38
7.2 Formal semantics 	                            40
7.3 Derived expression types 	                    43
Notes 	                                          45
Additional material 	                            45
Example 	                                        45
References 	                                      46
Alphabetic index of definitions of concepts,
keywords, and procedures 	                        48
```
