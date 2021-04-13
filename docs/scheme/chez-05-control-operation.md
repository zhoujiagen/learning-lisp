# 5 Control Operations
## 5.1 Procedure Application

任何第一个位置不是语法关键的结构化形式是过程应用:

``` scheme
syntax: (expr0 expr1 ...)
returns: values of applying the value of expr0 to the values of expr1 ...
```

过程和参数表达式的求值顺序是未描述的.

``` scheme
syntax: (apply procedure obj ... list) 
returns: the values of applying procedure to obj ... and the elements of list
libraries: (rnrs base), (rnrs)
```

`apply`调用`procedure`, 将第一个`obj`作为第一个参数传入, 第二个`obj`作为第二个参数传入, 以此类推, 将`list`中的元素按序作为剩余参数参数.


> 例: procedure-application.ss

## 5.2 Sequencing

``` scheme
syntax: (begin expr1 expr2 ...)
returns: the values of the last subexpression
libraries: (rnrs base), (rnrs)
```

表达式`expr1 expr2 ...`按从左至右的顺序求值.

## 5.3 Conditionals

``` scheme
syntax: (if test consequent alternative)
syntax: (if test consequent) 
returns: the values of consequent or alternative depending on the value of test
libraries: (rnrs base), (rnrs)
```

子形式`test`、`consequent`、`alternative`必须是表达式. 如果`test`求值为真(`#f`之外的任何值), 求值`consequent`并返回求值结果; 否则求值`alternative`并返回求值结果.

第二种情况没有`alternative`, `test`求值为假时结果是未描述的.

``` scheme
syntax: (not obj) 
returns: #t if obj is false, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (and expr ...) 
syntax: (or expr ...)
returns: 略
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (cond clause1 clause2 ...) 
returns: 略
libraries: (rnrs base), (rnrs)
```

除了最后一个, 每个`clause`子句有形式:

``` scheme
(test)
(test expr1 expr2 ...)
(test => expr)          ; expr的值是单参数的过程, 返回应用该过程到test上的结果值
```

最后一个子句还有`else`形式:

``` scheme
(else expr1 expr2 ...)
```

每个`test`被依次求值, 直到有一个求值为真, 或者所有`test`均已求值. 在第一个`test`求值为真的子句中, 表达式`expr`被求值.

> see [2.7 Conditional Expressions](/scheme/chez-02-getting-started/#27-conditional-expressions)

``` scheme
syntax: else
syntax: => 
libraries: (rnrs base), (rnrs exceptions), (rnrs)
```

``` scheme
syntax: (when test-expr expr1 expr2 ...)
syntax: (unless test-expr expr1 expr2 ...)
returns: 
libraries: (rnrs control), (rnrs)
```

``` scheme
syntax: (case expr0 clause1 clause1 ...)
returns: 
libraries: (rnrs base), (rnrs)
```

## 5.4 Recursion and Iteration

``` scheme
syntax: (let name ((var expr) ...) body1 body2 ...)
returns: values of the final body expression
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (do ((var init update) ...) (test result ...) expr ...) 
returns: the values of the last result expression
libraries: (rnrs control), (rnrs)
```

## 5.5 Mapping and Folding

``` scheme
syntax: (map procedure list1 list2 ...) 
returns: list of results
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (for-each procedure list1 list2 ...)
returns: unspecified
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (exists procedure list1 list2 ...) 
returns: 
libraries: (rnrs lists), (rnrs)
```

``` scheme
syntax: (for-all procedure list1 list2 ...) 
returns: 
libraries: (rnrs lists), (rnrs)
```

``` scheme
syntax: (fold-left procedure obj list1 list2 ...)
syntax: (fold-right procedure obj list1 list2 ...)
returns: 
libraries: (rnrs lists), (rnrs)
```

``` scheme
syntax: (vector-map procedure vector1 vector2 ...)
returns: vector of results
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (vector-for-each procedure vector1 vector2 ...) 
returns: unspecified
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (string-for-each procedure string1 string2 ...) 
returns: unspecified
libraries: (rnrs base), (rnrs)
```

## 5.6 Continuations

``` scheme
syntax: (call/cc procedure) 
syntax: (call-with-current-continuation procedure)
returns: 
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (dynamic-wind in body out)
returns: values resulting from the application of body
libraries: (rnrs base), (rnrs)
```

## 5.7 Delayed Evaluation

``` scheme
syntax: (delay expr) 
returns: a promise
procedure: (force promise)
returns: result of forcing promise
libraries: (rnrs r5rs)
```

## 5.8 Multiple Values

``` scheme
syntax: (values obj ...)
returns: obj ...
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (call-with-values producer consumer)
returns: 
libraries: (rnrs base), (rnrs)
```

## 5.9 Eval

``` scheme
syntax: (evl obj environment) 
returns: values of the Scheme expression represented by obj in environment
libraries: (rnrs eval)
```

``` scheme
syntax: (environment import-spec ...)
returns: an environment
libraries: (rnrs eval)
```

``` scheme
procedure: (null-environment version)
procedure: (scheme-report-environment version)
returns: an R5RS compatibility environment
libraries: (rnrs r5rs)
```