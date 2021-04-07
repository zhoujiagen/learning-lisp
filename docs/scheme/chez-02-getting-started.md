
### 2.1 Interacting with Scheme

REPL: read-evalute-print loop.

`define`建立变量绑定, `lambda`创建过程:

``` scheme
;;; square.ss
(define square
  (lambda (n)
    (* n n)))
```

加载文件:

``` scheme
(load "square.ss")
```

### 2.2 Simple Expressions

过程应用(procedure application): `(procedure arg ...)`.

对象的列表: `(obj1 obj2 ...)`.

`quote`: 显式的告知Scheme将列表视为数据而不是过程应用.<br/>

``` scheme
(quote (1 2 3 4))
; 简写形式
'(1 2 3 4)

(quote hello)
'2
'2/3
(quote "hi")
```

列表操作: `car`, `cdr`, `cons`.<br/>
`cons`: 构造对(pair).<br/>
合式列表(proper list): 列表的最后一个对的`cdr`是空列表;<br/>
非合式列表(improper list): 以点对(dotted-pair)记法打印.

``` scheme
(cons 'a 'b) ; (a . b)
```

<div>
{% dot list.svg
    digraph list {
      rankdir=LR;
      node [shape=record, width=1, height=.1];
      a [shape=record, label="{a|}"];
      b [shape=record, label="{b|}"];
      c [shape=record, label="{c|}"];
      d [shape=record, label="{d|( )}"];

      a -> b;
      b -> c;
      c -> d;

      ab [shape=record, label="{a|b}"];
    }
%}
</div>

### 2.3 Evaluating Scheme Expressions

```
(procedure arg1 ... argn)
```

- Scheme求值起可以按任意顺序求值这些表达式: `procedure`, `argi`;
- `procedure`的求值方式与`argi`相同.

核心句法形式(core syntactic form): 常量对象, 过程应用, `quote`表达式等;<br/>
句法扩展(syntactic extension): 用核心句法形式定义的句法形式.

### 2.4 Variables and Let Expressions

`let`表达式: 句法形式`let`绑定变量

``` scheme
; expr: 表达式
; var: 变量
(let ((var expr) ...) 
  body1 
  body2 
  ...)

(let [(var expr) ...] body1 body2 ...)
```

- `let`绑定的变量比在`let`的体中可见;
- 嵌套的`let`表达式: 在内部`let`表达式中绑定了与外部`let`相同的变量, 内部`let`的体中只可见内部`let`创建的绑定;
- 内部的绑定遮盖(shadow)了外部的绑定;
- 作用域(scope): 变量绑定可见的区域;
- 词法作用域(lexical scoping): 每个绑定的作用域可以通过直接文本分析程序获得.

### 2.5 Lambda Expressions

`lambda`表达式: 创建新的过程

``` scheme
; (var ...)不一定是合式列表:
; (1) 变量的合式列表: (var1 ... varn)
; (2) 单个变量: varr
; (3) 变量的非合式列表: (var1 ... varn . varr)

(lambda (var ...)
  body1
  body2
  ...)
```

应用`lambda`表达式:

``` scheme
((lambda (x) (+ x x)) (* 3 4)) ; 24
```

`lambda`表达式是对象(过程是对象):

``` scheme
(let ([double (lambda (x) (+ x x))])
  (list (double (* 3 4))))
```

`x`在`lambda`表达式中自由出现, 是自由变量(free variable); `y`不是:

``` scheme
(let ([x 'a])
  (let ([f (lambda (y) (list x y))])
    (f 'b)))
```

`let`表达式是`lambda`和过程应用定义的句法扩展:

``` scheme
(let ((var expr) ...) body1 body2 ...)
((lambda (var ...) body1 boyd2 ...)
  expr ...)
```

### 2.6 Top-Level Definitions

`define`创建顶级定义(top-level definition):

``` scheme
(define var expr)
```

``` scheme
; 定义过程
(define double-any
  (lambda (f x)
    (f x x)))

; 定义对象
(define sandwich "peanut-butter-and-jelly")
```

`define`的defun语法: 当`expr`是一个`lambda`表达式时

``` scheme
(define var0 (lambda (var1 ... varn) e1 e2 ...))
(define (var0 var1 ... varn) e1 e2 ...)

(define var0 (lambda varr) e1 e2 ...)
(define (var0 . varr) e1 e2 ...)

(define var0 (lambda var1 ... varn . varr) e1 e2 ...)
(define (var0 var1 ... varn . varr) e1 e2 ...)
```

在`lambda`表达式中未定义的变量, 在结果过程被实际应用之前, 不应该导致异常:

``` scheme
(define proc1 (lambda (x y) (proc2 y x)))

(define proc2 cons)
(proc1 'a 'b) ; (b . a)
```

### 2.7 Conditional Expressions

`if`表达式: 使用`if`句法形式

``` scheme
(if test consequent alternative)
```

真假值: 只有`#f`视为假, 其他对象均视为真.

`or`表达式: 

``` scheme
; (or): #f
; 按序对expr求值:
; (a) 有一个expr求值为真: 该expr的值
; (b) 耗尽expr: #f

(or expr ...)
```

`and`表达式:

``` scheme
; (and): #t
; 按序对expor求值:
; (a) 有一个expr求值为假: #f
; (b) 耗尽expr: 最后一个表达式的值
(and expr ...)
```

`not`: 真假值取反.

谓词: `=`, `<`, `>`, `<=`, `>=`, `null?`, `eqv?`等.

类型谓词: `pair?`, `symbol?`, `number?`, `string?`等.


`cond`表达式: 多个test和alternative

``` scheme
(cond (test expr) ... (else expr))
```

``` sheme
(define sign
  (lambda (n)
    (cond
      [(< n 0) -1]
      [(> n 0) +1]
      [else 0])))
```

### 2.8 Simple Recursion

递归过程(recursive procedure): 应用自身的过程.<br/>
两个基本元素: base case, recursion step.

``` scheme
(define length
    (lambda (lst)
        (if (null? lst)
            0
            (+ (length (cdr lst)) 1))))
```

Trace:

``` scheme
(trace length)

|(length (a b c d))
| (length (b c d))
| |(length (c d))
| | (length (d))
| | |(length ())
| | |0
| | 1
| |2
| 3
|4
4
```

迭代构造(iteration construct)用递归表示: list, pair. <br/>
特殊的迭代形式: `map`

``` scheme
(define abs
    (lambda (x)
        (if (< x 0)
            (- 0 x)
            x)))

(map abs '(1 -2 3 -4 5 -6)) ; (1 2 3 4 5 6)            
```

### 2.9 Assignment

赋值(assignment)并不像使用`let`和`lambda`创建新的绑定(binding), 而是使用`set!`修改既有绑定的值.

所有局部变量在绑定时立即赋予一个值.

赋值通常用于实现一些必须维护内部状态的过程:

> Example: stack.ss

``` scheme
(define next 0)
(define count
    (lambda ()
        (let ([v next])
            (set! next (+ next 1))
            v)))

(count) ; 0
(count) ; 1        
```

设置pair的car和cdr: `set-car!`, `set-cdr!`.

> Example: queue.ss
