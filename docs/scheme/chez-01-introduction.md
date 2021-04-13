# 1 Introduction

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
