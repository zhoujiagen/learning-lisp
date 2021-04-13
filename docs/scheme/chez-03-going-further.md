# 3 Going Further
## 3.1 Syntactic Extension

核心句法形式(core syntactic forms)包括:

- 顶层`define`形式
- 常量
- 变量
- 过程应用
- `quote`表达式
- `lambda`表达式
- `if`表达式
- `set!`表达式

使用这些定义和表达式描述Scheme的核心语法:

```
; 程序
<program>               --> <form>*
; 形式
<form>                  --> <definition> 
                            | <expression>
; 定义
<definition>            --> <variable definition>
                            | (begin <definition>*)
; 变量定义
<variable definition>   --> (define <variable> <expression>)
; 表达式
<expression>            --> <constant>
                            | <variable>
                            | (quote <datum>)
                            | (lambda <formals> <expression> <expression>*)
                            | (if <expression> <expression> <expression>)
                            | (set! <variable> <expression>)
                            | <application>
; 常量
<constant>              --> <boolean> | <number> | <character> | <string>
; 参数
<formals>               --> <variable>
                            | (<variable>*)
                            | (<variable> <variable>* . <variable>)
; 过程应用
<application>           --> (<expression> <expression>*)
```

其中:

- `|`表示选择, `*`表示零次或多次出现;
- `<variable>`是Scheme标识符;
- `<dataum>`是任意Scheme对象, 例如数字、列表、符号或向量等;
- `<boolean>`是`#t`或`#f`;
- `<number>`是任意数字;
- `<character>`是任意字符;
- `<string>`是任意字符串;
- `begin`表达式`(begin e1 e2 ...)`等价于`((lambda () e1 e2 ...))`.

句法形式可以在预期表达式或定义出现的位置出现, 只要扩展的形式恰当的展开为表达式或定义.

`define-syntax`定义句法扩展, 将句法转换过程(transformer)关联到一个关键字. 例:

``` scheme
(define-syntax let                          ; keyword
  (syntax-rules ()                          ; transformer, auxiliary keywords
    [(_ ((x e) ...) b1 b2 ...)              ; rules or pattern/template pairs
     ((lambda (x ...) b1 b2 ...) e ...)]))
```

`let*`: 与`let`类似, 但按顺序求值绑定, 之前的绑定在后续的绑定表达式中可见.

## 3.2 More Recursion

``` scheme
(let ((var expr) ...) body1 body2 ...)
(letrec ((var expr) ...) body1 body2 ...)
(let name ((var expr) ...) body1 body2 ...) ; named let expression
```

- `let`: `var ...`只在`bodyi`中可见;
- `letrec`: `var ...`在`bodyi`中可见, 在`expr ...`中可见; 必须能够在未求值任何`var ..`之前可以对每个`expr`求值;
- 命名的`let`: `var ...`只在`bodyi`中可见, 在`bodyi`中`name`绑定到一个可以递归调用的过程, 等价于:

``` scheme
((letrec [(name (lambda (var ...) body1 body2 ...))]
  name)
  expr ...)

(letrec [(name (lambda (var ...) body1 body2 ...))]
  (name expr ...))
```

尾递归(tail recursion): 一个过程尾调用(tail-call)自身, 或者通过一系列尾调用间接调用自身.

在`lambda`表达式中一个调用在尾部位置(in tail position): 调用的结果值作为`lambda`表达式的返回值返回.

``` scheme
; recursive version
(define factorial
  (lambda (n)
    (let fact ([i n])
      (if (= i 0)
          1
          (* i (fact (- i 1)))))))

; trace
|(fact 10)
| (fact 9)
| |(fact 8)
| | (fact 7)
| | |(fact 6)
| | | (fact 5)
| | | |(fact 4)
| | | | (fact 3)
| | | | |(fact 2)
| | | | | (fact 1)
| | | |[10](fact 0)
| | | |[10]1
| | | | | 1
| | | | |2
| | | | 6
| | | |24
| | | 120
| | |720
| | 5040
| |40320
| 362880
|3628800
```

``` scheme
; iterative version
(define factorial
  (lambda (n)
    (let fact 
      ([i n]  ; iter
      [a 1])  ; acc
      (if (= i 0)
          a
          (fact (- i 1) (* a i))))))

; trace
|(fact 10 1)
|(fact 9 10)
|(fact 8 90)
|(fact 7 720)
|(fact 6 5040)
|(fact 5 30240)
|(fact 4 151200)
|(fact 3 604800)
|(fact 2 1814400)
|(fact 1 3628800)
|(fact 0 3628800)
|3628800
```

## 3.3 Continuations

求值表达式时需要关注的事情:

- 求值什么(what to evaluate);
- 使用这个值做什么(what to do with the value): 称为计算的延续(the continuation of a computation).

在表达式求值的过程中任意时间点, 存在continuation准备好完成, 或者至少可以继续从该时间点开始的计算.

例: `x`有值`(a b c)`时, 求值`(if (null? x) (quote ()) (cdr x))`, continuation等待

- `(if (null? x) (quote ()) (cdr x))`的值;
- `(null? x)`的值;
- `null?`的值;
- `x`的值;
- `(cdr x)`的值: 与整个表达式相同;
- `cdr`的值;
- `x`的值.

过程`call/cc`:

- 使用`call/cc`捕获任意表达式的continuation;
- `call/cc`的参数是一个过程`p`;
- `call/cc`构造出当前continuation的具体表示, 并传递给`p`; 这个continuation本身被表示为过程`k`;
- 每次`k`应用到一个值时, 返回该值到`call/cc`应用的continuation; 这个值成为`call/cc`应用的值;
- 如果`p`在没有调用`k`的情况下返回, 返回的值成为`call/cc`应用的值.


> Example: non-local exit

``` scheme
(define (product lst)
    (call/cc 
        (lambda (break)
            (let f ([lst lst])
                (cond
                    [(null? lst) 1]
                    [(= (car lst) 0) (break 0)] ; invoke continuation
                    [else (* (car lst) (f (cdr lst)))]))))) 
```

> Example: puzzling examples

``` scheme
(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))
(((call/cc (lambda (k) k)) (lambda (x) x)) "hi")
```

## 3.4 Continuation Passing Style

通常, 每个过程调用都会关联一个continuation:

- 非尾部调用: 被调用过程接收一个隐式的continuation, 该continuation负责完成调用过程体和返回到调用过程的continutation.
- 尾部调用: 被调用的过程接收调用过程的continuation.

传递continuation风格(CPS): 将过程调用中的隐式continuation以参数形式表示.

> Example: CPS

``` scheme
(letrec ([f (lambda (x) (cons 'a x))]
        [g (lambda (x) (cons 'b (f x)))]
        [h (lambda (x) (g (cons 'c x)))])
    (displayln (cons 'd (h '())))) ; (d b a c)

(letrec ([f (lambda (x k) (k (cons 'a x)))]
        [g (lambda (x k)
            (f x (lambda (v) (k (cons 'b v)))))]
        [h (lambda (x k) (g (cons 'c x) k))])
    (displayln (h '() (lambda (v) (cons 'd v))))) ; (d b a c)
```

## 3.5 Internal Definitions

内部定义: 定义可以出现在`lambda`、`let`或`letrec`的体中, 创建的绑定的作用域在出现的体中.

使用内部定义创建的绑定可以是互递归的(mutually recursive).

下面的形式或表达式等价:

``` scheme
; 体
(define var expr0)
...
expr1
expr2
...

(letrec* ((var expr0) ...) expr1 expr2 ...)

(let ()
  (define var expr0)
  ...
  expr1
  expr2
  ...)
```

`letrec*`: 类似与`lect*`, 要求从左向右的求值顺序.

`case`表达式: 

``` scheme
(case <key> <case clause1> <case clause2> ...)    ; syntax 

; <Key> must be an expression. 
; Each <case clause> must have one of the following forms:
((<datum1> ...) <expression1> <expression2> ...)
(else <expression1> <expression2> ...)
```

## 3.6 Libraries

> Example: grade library

``` scheme
(library (grades)
  (export gpa->grade gpa)
  (import (rnrs))
  (define gpa->grade ...)
  (define-syntax gpa ...))

(import (grades))
(gpa ...)
```
