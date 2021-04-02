# Chez Scheme

|#|Title|Progress|Description|
|:---|:---|:---|:---|
|1|Introduction|100%|20210327|
|2|Getting Started|100%|20210328|
|3|Going Further|||
|4|Procedures and Variable Bindings|||
|5|Control Operations|||
|6|Operations on Objects|||
|7|Input and Output|||
|8|Syntactic Extension|||
|9|Records|||
|10|Libraries and Top-Level Programs|||
|11|Exceptions and Conditions|||
|12|Extended Examples|||

## 术语

<!-- 记录阅读过程中出现的关键字及其简单的解释. -->

## 介绍

<!-- 描述书籍阐述观点的来源、拟解决的关键性问题和采用的方法论等. -->

## 动机

<!-- 描述阅读书籍的动机, 要达到什么目的等. -->

## 概念结构

<!-- 描述书籍的行文结构, 核心主题和子主题的内容结构和关系. -->

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=spline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    
    c1 [label="Introduction"];
    c1_concepts [shape=record, label="
    naming convention\l
    | notation convention\l
    "]
    c1 -> c1_concepts;
    
    c2 [label="Getting Started"];
    c2_concepts [shape=record, label="
    REPL\l
    | define\l
    | load\l
    | (procedure arg...)\l
    | quote\l
    | car, cdr, cons, proper list\l
    | evaluation expressions\l
    | let\l
    | lambda\l
    | if, or, and, not, cond\l
    | simple recursion, map, trace\l
    | assignment: set!, set-car!, set-cdr!\l
    "]
    c2 -> c2_concepts;
    
    c3 [label="Going Further"];
    c3_concepts [shape=record, label="
    <ss> syntactic extension\l
    | more recursion: letrec, named let, tail recursion\l
    |<cont> continutation\l
    | internal definition\l
    | library\l
    "]
    c3 -> c3_concepts;
    
    syntactic_extension [shape=record, label="
    core syntactic form\l
    | form, definition, expression, application\l
    | begin\l
    | define-syntax, syntax-rules\l
    "]
    c3_concepts:ss -> syntactic_extension;
    
    continuations [shape=record, label="
    continuation passing styles\l
    "]
    c3_concepts:cont -> continuations;
    
    
    c4 [label="Procedures and Variable Bindings"];
    c5 [label="Control Operations"];
    c6 [label="Operations on Objects"];
    c7 [label="Input and Output"];

    c8 [label="Syntactic Extension"];
    c8_concepts [shape=record, label="
    keyword - transformer: define-syntax, let-syntax, letrec-syntax\l
    | <transfomers> transfomer\l
    | <expanders> expander\l
    "]    
    c8 -> c8_concepts;
    
    transfomers [shape=record, label="
    syntax-rules\l
    | syntax-case, syntax\l
    | identifier-syntax, make-variable-transformer
    "]
    c8_concepts:transfomers -> transfomers;
    
    expanders [shape=record, label="
    left to right\l
    | variable definition\l
    | keyword definition\l
    | expression\l
    "]
    c8_concepts:expanders -> expanders;

    c9 [label="Records"];
    c10 [label="Libraries and Top-Level Programs"];
    c11 [label="Exceptions and Conditions"];
    c12 [label="Extended Examples"];
    
    
    root -> {c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12} [style=invis]

}
%}
</div>

## 1 Introduction   

标识符(identifier): 字母、数字、特殊字符(`?!.+-*/<=>:$%^&_~@`)和Unicode字符; 大小写敏感.

带结构的形式(form)和列表(list)常量用括号`()`包裹, `[]`可以用在`()`可以出现的地方.

向量(vector)用`#()`包裹, 字节向量(bytevector)用`#vu8()`包裹.

字符串用`""`包裹, 字符以`#\`开始.

数字:<br/>
整数: `-123`<br/>
分数: `1/2`<br/>
浮点数: `1.3`<br/>
科学记数法: `1e23`<br/>
复数: `1.3-2.7i`, `-1.2@73`.

布尔值: `#t`, `#f`.

注释:<br/>
行注释: `;`<br/>
过程(procedure)注释: `;;;`<br/>
块(block)注释: `#|`, `#|`<br/>
数据项(datum)注释`#;`.

不可打印的表示: `#<procedure>`, `#<port>`.

命名约定:

- 谓词(predicate)以`?`结尾: `eq?`, `zero?`, `string=?`;
- 类型谓词(type predicate)用类型加上`?`表示: `pair?`;
- 字符、字符串和向量过程分别以`char-`、`string-`、`vector`开始;
- 转换对象类型的过程用`type1->type2`表示: `vector->list`;
- 产生副作用的过程和句法形式(syntactic form)的名称以`!`结尾: `set!`, `vector-set!`.

记法约定:

- `unspecified`: 只用于产生副作用的标准过程或句法形式的返回值是未描述的;
- `syntax violation`: 描述程序是形式错误的, 在句法形式的结构不匹配它的原型时发生;
- `...`用于表示子表达式(subexpression)或参数(argument)的零次或多次出现.

## 2 Getting Started   

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

## 3 Going Further
### 3.1 Syntactic Extension

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
                            | (quote <datum>) ; datum: 任意Scheme对象
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

### 3.2 More Recursion

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

### 3.3 Continuations

求值表达式时需要关注的事情:

- 求值什么(what to evaluate);
- 使用这个值做什么(what to do with the value): 称为计算的延续(the continuation of a computation).

在表达式求值的过程中任意时间点, 存在continuation准备好完成, 或者至少可以继续从该时间点开始的计算.

例: 求值`(if (null? x) (quote ()) (cdr x))`, continuation等待

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

### 3.4 Continuation Passing Style

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


### 3.5 Internal Definitions
### 3.6 Libraries

## 4 Procedures and Variable Bindings
### 4.1 Variable References
### 4.2 Lambda
### 4.3 Case-Lambda
### 4.4 Local Binding
### 4.5 Multiple Values
### 4.6 Variable Definitions
### 4.7 Assignment

## 5 Control Operations
### 5.1 Procedure Application
### 5.2 Sequencing
### 5.3 Conditionals
### 5.4 Recursion and Iteration
### 5.5 Mapping and Folding
### 5.6 Continuations
### 5.7 Delayed Evaluation
### 5.8 Multiple Values
### 5.9 Eval

## 6 Operations on Objects
### 6.1 Constants and Quotation
### 6.2 Generic Equivalence and Type Predicates
### 6.3 Lists and Pairs
### 6.4 Numbers
### 6.5 Fixnums
### 6.6 Flonums
### 6.7 Characters
### 6.8 Strings
### 6.9 Vectors
### 6.10 Bytevectors
### 6.11 Symbols
### 6.12 Booleans
### 6.13 Hashtables
### 6.14 Enumerations

## 7 Input and Output
### 7.1 Transcoders
### 7.2 Opening Files
### 7.3 Standard Ports
### 7.4 String and Bytevector Ports
### 7.5 Opening Custom Ports
### 7.6 Port Operations
### 7.7 Input Operations
### 7.8 Output Operations
### 7.9 Convenience I/O
### 7.10 Filesystem Operations
### 7.11 Bytevector/String Conversions

## 8 Syntactic Extension

**形式**

```
(keyword subform ...) | improper list | singleton identifier
```

- `keyword`: 句法扩展的名称.
- 用关键字与转换过程(transformer)的关联来定义: `define-syntax`、`let-syntax`、`letrec-syntax`.
- 由语法展开器(syntax expander)在求值开始时(在编译或解释之前), 展开为核心形式.

**创建transformer**

(1) `syntax-rules`: 简单的基于模式的转换

(2) 常规的过程, 接受单个参数, 执行任意计算

- `syntax-case`: 解构(destructure)转换的输入.
- `syntax`: 构造转换的输出.

(3) 匹配单个标识符, 并赋值

- `identifier-syntax`形式: 使用类似与`syntax-rules`中的简单模式.
- `make-variable-transformer`过程: 执行任意计算.

**展开器**

展开器从左向右处理`library`、`lambda`或其他体中的初始化形式. 遇到:

- 变量定义: 记录该定义的标识符是一个变量, 延迟右侧表达式的展开, 直到处理完所有的定义.
- 关键字定义: 展开并求值右侧的表达式, 将关键字绑定到结果transformer.
- 表达式: 完全展开所有延迟的右侧表达式、当前和剩下的体表达式.


### 8.1 Keyword Bindings


(1) `define-syntax`

``` scheme
(define-syntax keyword expr)
```

- `expr`必须求值为transformer.

(2) `let-syntax`, `letrec-syntax`

``` scheme
(let-syntex ((keyword expr) ...) form1 form2 ...)
(letrec-syntax ((keyword expr) ...) form1 form2 ...)
```

- `expr`必须求值为transformer.
- `keyword`均在`form1` `form2` `...`中绑定, 在`letrec-syntax`中的`expr`中绑定.


### 8.2 Syntax-Rules Transformers


``` scheme
(syntax-rules (literal ...) clause ...)
```

- `literal`: 字面量, 除了`_`或`...`之外的标识符; 充当辅助关键字, 例如`case`和`cond`表达式中的`else`.
- `clause`的形式: `(pattern template)`; 模式`pattern`描述输入的形式的语法, 模板`template`描述输出的形式.

`pattern`的详细说明:

- `pattern`由列表结构、向量结构、标识符和常量构成.
- `pattern`中的标识符可以是字面量、模式变量、下划线(`_`)、省略号(ellipsis, `...`). 如果标识符不是`_`或`...`, 且在`(literal ...)`中出现, 则它是一个字面量.
- `pattern`中的列表结构和向量结构描述了输入的形式所需的基本结构; `_`和模式变量描述了任意结构; 字面量和常量描述了必须精确匹配的原子片段; `...`描述了在它之前的子模式重复出现.

输入形式`F`匹配模式`P`, 当且仅当:

- `P`是`_`或模式变量;
- `P`是标识符, `F`是与`P`标识符具有相同绑定的标识符(用谓词`free-identifier=?`确定);
- `P`有形式`(P1 ... Pn)`, `F`是有`n`个元素的列表, 分别匹配`P1`到`Pn`;
- `P`有形式`(P1 ... Pn . Px)`, `F`是有`n`个或更多元素的列表(不管是否是合式的列表), 头`n`个元素分别匹配`P1`到`Pn`, 第`n`个`cdr`匹配`Px`;
- `P`有形式`(P1 ... Pk Pe ellipsis Pm+1 ... Pn)`, `ellipsis`是标识符`...`, `F`是有`n`个元素的合式列表, 头`k`个元素分别匹配`P1`到`Pk`, 后面`m-k`个元素均分别匹配`Pe`, 余下的`n-m`个元素分别匹配`Pm+1`到`Pn`;
- `P`有形式`(P1 ... Pk Pe ellipsis Pm+1 ... Pn . Px)`, `F`是有`n`个或更多元素的列表(不管是否是合式的列表), 头`k`个元素分别匹配`P1`到`Pk`, 后面`m-k`个元素均分别匹配`Pe`, 后面`n-m`个元素分别匹配`Pm+1`到`Pn`, 第`n`个`cdr`匹配`Px`;
- `P`有形式`#(P1 ... Pn)`, `F`是有`n`个元素的向量, 分别匹配`P1`到`Pn`;
- `P`有形式`#(P1 ... Pk Pe ellipsie Pm+1 ... Pn)`, `F`是有`n`个或更多元素的向量, 头`k`个元素分别匹配`P1`到`Pk`, 后面`m-k`个元素均分别匹配`Pe`, 余下的`n-m`各元素分别匹配`Pm+1`到`Pn`;
- `P`是模式数据项(datum, 任意的非列表、非向量、非符号的对象), `F`在`equal?`过程含义下等价于`P`.

`syntax-rules`中`pattern`的最外层结构必须是上述的列表结构形式, 而子模式可以是上述任一形式. 最外层模式的首元素总是被认为是命名句法形式的关键字, 故而总被忽略,

如果传递给`syntax-rules` transformer的输入形式匹配某一子句的模式, 则接受该子句, 根据相应的模板转换输入形式.

模板可以是一个模式变量、一个不是模式变量的标识符、一个模式数据项、子模板的列表`(S1 ... Sn)`、子模板的非合式列表`(S1 S2 ... Sn . T)`、子模板的向量`#(S1 ... Sn)`.

转换:

- 模板中模式变量用其绑定的输入子形式替换;
- 模式数据和不是模式变量的标识符保持原样;
- 模板中列表和向量结构保持列表和向量结构;
- 后跟`...`的子模板展开为零个或多个子模板;
- 子模板中后跟`...`的子模式中必须包含至少一个模式变量;
- 在后跟一个或多个`...`的子模式中出现的模式变量, 只可以在后跟至少相同数量的`...`的子模式中出现;
- 如果模板中一个模式变量后的`...`数量多于相应模式中`...`的数量, 则输入形式按需重复;
- 形式为`(... template)`的模板等同于`template`, 除非模板中的`...`有特殊的含义.


``` scheme
_
...
```

- `_`是`syntax-rules`、`identifier-syntax`、`syntax-case`的辅助关键字;
- `...`是`syntax-rules`、`identifier-syntax`、`syntax-case`、`syntax`、`quasisyntax`的辅助关键字.


``` scheme
(identifier-syntax tmpl)
(identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))
```

- 当一个关键字绑定到由`(identifier-syntax tmpl)`生成的transformer时, 在绑定作用域内对关键字的引用被替换为`tmpl`. 不允许用`set!`对相应的关键字赋值.
- `(identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))`则允许transformer指定使用`set!`时的操作.


### 8.3 Syntax-Case Transformers


`syntax-case`是`syntax-rules`的泛化版本.

使用这种机制时, transformer是一个单参数的过程: 这个参数是语法对象(syntax object), 表示要处理的形式; 返回值是表示输出形式的语法对象. 通常用`syntax-case`解构输入, 使用`syntax`重建输出.

语法对象:

- 一个非对(pair)、非向量、非符号的值;
- 语法对象的对;
- 语法对象的向量;
- 包裹对象.

包裹了语法对象的对象包含了一个形式的结构和上下文信息, 展开器使用上下文信息维护词法作用域. 所有标识符的上下文信息必须存在, 表示一个标识的语法对象本身也被引用为一个标识符; 所以标识符表示: (1) 句法实体(符号、变量、关键字), (2) 句法对象的语法对象具体表示.

``` scheme
(syntax-case expr (literal ...) clause ...)
```

- `literal`必须是一个标识符;
- `clause`有两个形式: `(pattern output-expression)`、`(pattern fender output-expression)`;
- `syntax-case`的模式与`syntax-ruels`中模式相同;

`syntax-case`首先求值`expr`, 尝试将结果值与第一个`clause`中的模式匹配. 这个结果值可以是任意Scheme对象.

- 如果结果值匹配模式且没有`fender`, 则求值`output-expression`, 将其结果值作为`syntax-case`表达式的值返回;
- 如果结果值不匹配模式, 则将结果值与下一个`clause`比较, 依次类推.
- 如果有`fender`, 它作为接受`clause`的额外约束. 如果`expr`的值匹配某个`clause`, 相应的`fender`被求值: 如果`fender`求值结果为真, 则接受该`clause`, 否则拒接接受该`clause`.

`clause`的`pattern`中的模式变量在`clause`的`fender`和`output-expression`中绑定到相应的输入值. 模式变量与程序变量和关键字共用同一个命名空间, `syntax-case`创建的模式变量绑定可以遮盖(或被遮盖)程序变量和关键字绑定、其他模式变量绑定. 模式变量只能在`syntax`表达式中引用.


``` scheme
(syntax template)
#'template
```

`syntax`表达式与`quote`表达式类似, 除了:

- `template`中出现的模式变量的值被插入`template`中;
- 与输入和模板中关联的上下文信息在输出中保留, 以支持词法作用域;

这里的`template`与`syntax-rules`中的`template`等同. 模板中的列表和向量变为真正的列表和向量, 其中的模式变量用值替换.

``` scheme
(identifier? obj)
```

`identifier?`常用在`fender`中, 以检查输入形式的特定子形式是否是标识符.

``` scheme
(free-identifier=? identifier1 identifier2)
(bound-identifier=? identifier1 identifier2)
```

`free-identifier=?`和`bound-identifier=?`用于在特定上下文中根据标识符的用途(自由引用、绑定的标识符)比较标识符.

- `free-identifier=?`确定在transformer的输出中两个自由标识符是否等价: 当且仅当`id1`与`id2`引用同一绑定时, `(free-identifier=? id1 id2)为真. `syntax-case`模式中出现的字面量标识符按此匹配.
- `bound-identifier=?`确定在transformer的输出中两个绑定标识符是否等价: 如果返回真, 一个标识符的绑定将在其作用域内捕获另一个标识符引用. 通常, 只有两个标识符同时在原始程序中出现、或由同一个transformer引用引入时, 才会返回真.

``` scheme
(with-syntax ((pattern expr) ...) body1 body2 ...)
```

`with-syntax`允许创建局部模式绑定:

- `pattern`与`syntax-case`中`pattern`等同;
- 求值`expr`, 返回的结果值按相应的`pattern`解构, 与`syntax-case`类似, `pattern`中模式变量在`body1 body2 ...`中绑定到相应的值.


``` scheme
(quasisyntax template ...)
#`template

(unsyntax template ...)
#,template

(unsyntax-splicing template ...)
#,@template
```

`unsyntax`和`unsyntax-splicing`是`quasisyntax`的辅助关键字.


`quasisyntax`与`syntax`类似, 但允许摘录的文本中部分求值, 行为与`quasiquote`类似.

在`quasisyntax`的`template`中, `unsyntax`和`unsyntax-splicing`子形式被求值, 其他部分视为与`syntax`中常规模板元素相同. 每个`unsyntax`子形式的值在输出中替换`unsyntax`形式, `unsyntax-splicing`子形式的值被粘接在外围的列表或向量结构中. `unsyntax`和`unsyntax-splicing`指导`quansisyntax`表达式中有效.

`quasisyntax`表达式可以嵌套, 每个`quasisyntax`引入一层语法摘录, 每个`unsyntax`或`unsyntax-splicing`解除一层语法摘录.

包含零个或多个子形式的`unsyntax`和`unsyntax-splicing`形式在在粘接列表或向量上下文中有效:

- `(unsyntax template ...)`等价于`(unsyntax template) ...`;
- `(unsyntax-splicing template ...)`等价于`(unsyntax-splicing template) ...`.


``` scheme
(make-variable-transformer procedure)
```

传递给transformer的形式通常是一个关键字, 或者带括号的形式, 其第一个子形式是绑定到transformer的关键字. `make-variable-transformer`用于将一个过程转换为一种特殊的transformer, 在遇到在`set!`关键字之后立即出现的关键字时, 展开器也传入`set!`形式, 就像给变量赋值.


``` scheme
(syntax->datum obj)
(datum->syntax template-identifier obj)
```

`syntax->datum`从语法对象中移除所有句法信息, 返回相应的Scheme数据项. 按这种方式移除标识符, 返回其符号名称.

`datum->syntax`从包含与`template-identifier`相同上下文信息的`obj`构造语法对象, 表现为当`template-identifier`被引入时该语法对象被引入代码中. `template-identifier`通常是输入形式的关键字, `obj`通常是命名了要构造的标识符的符号.<br/>
`datum->syntax`允许transformer篡改词法作用域规则, 创建隐式的标识符, 就像这些标识符在输入形式中存在一样, 从而允许定义出引入不在输入形式中显式出现的标识符的可见绑定或引用的句法扩展.

``` scheme
(generate-temporaries list)
```

`generate-temporaries`用于构造临时标识符.

`list`可以是任意列表, 它的内容不重要. 生成的临时标识符的数量与`list`中元素数量相同, 每个临时标识符互不相同.


### 8.4 Examples

## 9 Records

> TODO(zhoujiagen) restart here! 2021-03-17

> abbreviation
> rtd: record type descriptor
> rcd: record constructor descriptor

record definition: defines

- a record type identified by `record-name`
- a constructor: default `make-record-name`
- a predicate: default `record-name?`
- assessors: default `record-name-field-name`
- mutators: default `record-name-field-name-set!`

``` scheme
(define-record-type record-name clause ...)
(define-record-type (record-name constructor pred) clause ...)
```

- Fields clause

``` scheme
(fields field-spec)

; field-spec
field-name
(immutable field-name)
(mutable field-name)
(immutable field-name accessor-name)
(mutable field-name accessor-name mutator-name)
```

- Parent clause

- Nongenerative clause

- Protocol clause

- Sealed clause

- Opaque clause

- Parent-rtd clause


### 9.1 Defining Records
### 9.2 Procedural Interface
### 9.3 Inspection

## 10 Libraries and Top-Level Programs
### 10.1 Standard Libraries
### 10.2 Defining New Libraries
### 10.3 Top-Level Programs
### 10.4 Examples

## 11 Exceptions and Conditions
### 11.1 Raising and Handling Exceptions
### 11.2 Defining Condition Types
### 11.3 Standard Condition Types

## 12 Extended Examples
### 12.1 Matrix and Vector Multiplication
### 12.2 Sorting
### 12.3 A Set Constructor
### 12.4 Word Frequency Counting
### 12.5 Scheme Printer
### 12.6 Formatted Output
### 12.7 A Meta-Circular Interpreter for Scheme
### 12.8 Defining Abstract Objects
### 12.9 Fast Fourier Transform
### 12.10 A Unification Algorithm
### 12.11 Multitasking with Engines  





## 总结

<!-- 概要记录书籍中如何解决关键性问题的. -->

## 应用

<!-- 记录如何使用书籍中方法论解决你自己的问题. -->

## 文献引用

<!-- 记录相关的和进一步阅读资料: 文献、网页链接等. -->

R. Kent Dybvig. The Scheme Programming Language. The MIT Press, 2009.

## 其他备注