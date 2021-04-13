# 4 Procedures and Variable Bindings
## 4.1 Variable References

``` scheme
syntax: variable
returns: the value of variable
```

## 4.2 Lambda

``` scheme
syntax: (lambda formals body1 body2 ...)
returns: a procedure
libraries: (rnrs base), (rnrs)
```

创建过程或建立局部变量绑定的任意操作最终是用`lambda`和`case-lambda`定义的.

`formals`中变量是过程的形式参数, 子形式序列`body1 body2 ...`是过程的体. 体可以以定义的序列开始, 这些定义创建的绑定的作用域在体中.

在过程应用时, `formals`定义的形式参数按如下方式与实际参数绑定:

- `formals`是变量的合式列表, 例如`(x y z)`, 每个变量绑定到相应的实际参数. 如果实际参数过多或过少, 抛出条件类型为`&assertion`的异常.
- `formals`是单个变量, 例如`z`, 它将绑定到实际参数的一个列表.
- `formals`是非合式列表, 例如`(x y . z)`, 除最后一个变量外的所有变量绑定到相应的实际参数, 最后一个变量绑定到剩余的实际参数的一个列表. 如果实际参数过少, 抛出条件类型为`&assertion`的异常.

当体被求值时, 体中的表达式按序求值, 过程返回最后一个表达式的值.

## 4.3 Case-Lambda

``` scheme
syntax: (case-lambda clause ...)
returns: a procedure
libraries: (rnrs control), (rnrs)
```

`case-lambda`句法形式直接支持可选参数的过程、有固定或不确定数量参数的过程.

子句`clause`的形式为`[formals body1 body2 ...]`. 当应用由`case-lambda`创建的过程时, 按序检查这些子句, 第一个接受指定数量实际参数的子句被选择. 

> 例: case-lambdas.ss

## 4.4 Local Binding

``` scheme
syntax: (let ((var expr) ...) body1 body2 ...) 
syntax: (let* ((var expr) ...) body1 body2 ...) 
syntax: (letrec ((var expr) ...) body1 body2 ...) 
syntax: (letrec* ((var expr) ...) body1 body2 ...) 
returns: the values of the final body expression
libraries: (rnrs base), (rnrs)
```

`let`建立局部变量绑定. 每个变量`var`绑定到相应的表达式`expr`的值. `let`的体是这些变量的作用域, 按`lambda`体的方式处理和求值.

`let*`与`let`类似, 除了`expr ...`按从左至右的顺序求值, 每个表达式在其左边的变量的作用域中.

`letrec`与`let`、`let*`类似, 除了所有表达式`expr ...`在所有变量`var ...`的作用域中. 表达式`expr ...`的求值顺序未描述(unspecified).

`letrec*`与`letrec`类似, 除了`expr ...`按从左至右的顺序求值.

## 4.5 Multiple Values

``` scheme
syntax: (let-values ((formals expr) ...) body1 body2 ...) 
syntax: (let*-values ((formals expr) ...) body1 body2 ...)
returns: the values of the final body expression
libraries: (rnrs base), (rnrs)
```

`let-values`接受多个值并将它们绑定到变量. `let*-values`执行与`let*`类似的从左至右顺序的绑定.

> 例: multiple-values.ss

## 4.6 Variable Definitions

``` scheme
syntax: (define var expr)
syntax: (define var) ; (define var unspecified)
syntax: (define (var0 var1 ...) body1 body2 ...)
syntax: (define (var0 . var1) body1 body2 ...)
syntax: (define (var0 var1 var2 ... . varr) body1 body2 ...)
libraries: (rnrs base), (rnrs)
```

定义可以出现在`library`体开始处、顶层程序的体中、`lambda`或`case-lambda`体的开始处、从`lambda`导出的任意形式的体(例如`let`或`letrec*`)的开始处.

## 4.7 Assignment

``` scheme
syntax: (set! var expr)
returns: unspecified
libraries: (rnrs base), (rnrs)
```

`set!`没有建立`var`的新绑定, 而是修改了既有绑定的值. 先求值`expr`, 在将`expr`的值赋给`var`.

> 例: assignments.ss