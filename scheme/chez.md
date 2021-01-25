# Chez Scheme

core syntax of Scheme:

```
<program>               --> <form>*
<form>                  --> <definition> | <expression>
<definition>            --> <variable definition> 
                            | (begin <definition>*)
; 变量定义
<variable definition>   --> (define <variable> <expression>)
<expression>            --> <constant>
                            | <variable> ; 标识符
                            | (quote <datum>) ; datum: 任意Scheme对象
                            | (lambda <formals> <expression> <expression>*)
                            | (if <expression> <expression> <expression>)
                            | (set! <variable> <expression>)
                            | <application>
<constant>              --> <boolean> | <number> | <character> | <string>
; 参数
<formals>               --> <variable>
                            | (<variable>*)
                            | (<variable> <variable>* . <variable>)
; 过程应用
<application>           --> (<expression> <expression>*)
```

## 句法扩展(Syntactic Extension)

### 形式

```
(keyword subform ...) | improper list | singleton identifier
```

- `keyword`: 句法扩展的名称.
- 用关键字与转换过程(transformer)的关联来定义: `define-syntax`、`let-syntax`、`letrec-syntax`.
- 由语法展开器(syntax expander)在求值开始时(在编译或解释之前), 展开为核心形式.

### 创建transformer

(1) `syntax-rules`: 简单的基于模式的转换

(2) 常规的过程, 接受单个参数, 执行任意计算

- `syntax-case`: 解构(destructure)转换的输入.
- `syntax`: 构造转换的输出.

(3) 匹配单个标识符, 并赋值

- `identifier-syntax`形式: 使用类似与`syntax-rules`中的简单模式.
- `make-variable-transformer`过程: 执行任意计算.

### 展开器

展开器从左向右处理`library`、`lambda`或其他体中的初始化形式. 遇到:

- 变量定义: 记录该定义的标识符是一个变量, 延迟右侧表达式的展开, 直到处理完所有的定义.
- 关键字定义: 展开并求值右侧的表达式, 将关键字绑定到结果transformer.
- 表达式: 完全展开所有延迟的右侧表达式、当前和剩下的体表达式.

### 关键字绑定

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


### 语法规则的transformer

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


### 语法案例的transformer


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