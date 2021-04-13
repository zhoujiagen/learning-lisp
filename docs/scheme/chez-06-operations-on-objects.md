# 6 Operations on Objects
## 6.1 Constants and Quotation

``` scheme
syntax: constant
returns: constant
```

``` scheme
syntax: (quote obj)
syntax: 'obj
returns: obj
libraries: (rnrs base), (rnrs)
```

``` scheme
syntax: (quasiquote obj ...)
syntax: `obj
syntax: (unquote obj ...)
syntax: ,obj
syntax: (unquote-splicing obj ...)
syntax: ,@obj
returns:
libraries: (rnrs base), (rnrs)
```

## 6.2 Generic Equivalence and Type Predicates

``` scheme
procedure: (eq? obj1 obj2) 
returns: #t if obj1 and obj2 are identical, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (eqv? obj1 obj2) 
returns: #t if obj1 and obj2 are equivalent, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (equal? obj1 obj2)
returns: #t if obj1 and obj2 have the same structure and contents, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (boolean? obj) 
returns: #t if obj is either #t or #f, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (null? obj)
returns: #t if obj is the empty list, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (pair? obj)
returns: #t if obj is a pair, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (number? obj)
returns: #t if obj is a number object, #f otherwise
procedure: (complex? obj)
returns: #t if obj is a complex number object, #f otherwise
procedure: (real? obj)
returns: #t if obj is a real number object, #f otherwise
procedure: (rational? obj)
returns: #t if obj is a rational number object, #f otherwise
procedure: (integer? obj)
returns: #t if obj is a integer object, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (real-valued? obj)
returns: #t if obj is a real number, #f otherwise
procedure: (rational-valued? obj)
returns: #t if obj is a rational number, #f otherwise
procedure: (integer-valued? obj)
returns: #t if obj is a integer, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (char? obj)
returns: #t if obj is a character, #f otherwise
procedure: (string? obj)
returns: #t if obj is a string, #f otherwise
procedure: (vector? obj)
returns: #t if obj is a vector, #f otherwise
procedure: (symbol? obj)
returns: #t if obj is a symbol, #f otherwise
procedure: (procedure? obj)
returns: #t if obj is a procedure, #f otherwise
procedure: (bytevector? obj)
returns: #t if obj is a bytevector, #f otherwise
procedure: (hashtable? obj)
returns: #t if obj is a hashtable, #f otherwise
libraries: (rnrs base), (rnrs)
libraries: (rnrs bytevectors), (rnrs)
libraries: (rnrs hashtables), (rnrs)
```

## 6.3 Lists and Pairs

``` scheme
procedure: (cons obj1 obj2)
returns: a new pair whose car and cdr are obj1 and obj2
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (car pair)
returns: the car of pair
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (cdr pair)
returns: the cdr of pair
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (set-car! pair obj) 
returns: unspecified
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (set-cdr pair obj)
returns: unspecified
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (caar pair)
procedure: (cadr pair)
...
procedure: (cddddr pair)
returns: the caar, cadr, ..., cddddr of pair
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (list obj ...)
returns: a list of obj ...
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (cons* obj ... final-obj)
returns: a list obj ... terminated by final-obj
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (list? obj)
returns: #t if obj is a proper list, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (length list)
returns: the number of elements in list
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (list-ref list n)
returns: the nth element (zero-based) of list
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (list-tail list n) 
returns: the nth tail (zero-based) of list
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (append)
procedure: (append list ... obj)
returns: the concatenation of the input lists
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (reverse list)
returns: a new list containing the elements of list in reverse order
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (memq obj list)      ; eq?
procedure: (memv obj list)      ; eqv?
procedure: (member obj list)    ; equal?
returns: the first tail of list whose car is equivalent to obj, or #f
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (memp procedure list)
returns: the first tail of list for whose car procedure returns true, or #f
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (remq obj list)
procedure: (remv obj list)
procedure: (remove obj list)
returns: a list containing the elements of list with all occurrences of obj removed
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (remp procedure list)
returns: a list of the elements of list for wich procedure returns #f
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (filter procedure list)
returns: a list of the elements of list for which procedure returns true
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (partition procedure list)
returns: 
libraries: (rnrs lists), (rnrs)
```

``` scheme
procedure: (find procedure list)
returns: the first element of list for which procedure returns true, or #f
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (assq obj alist)     ; eq?
procedure: (assv obj alist)     ; eqv?
procedure: (assoc obj alist)    ; equal?
returns: first element of alist whose car is equivalent to obj, or #f
libraries: (rnrs base), (rnrs)
```

`alist`必须是关联列表(association list). 关联列表是元素为键值对`(key . value)`的合式列表.

``` scheme
procedure: (assp procedure alist)
returns: first element of alist for whose car procedure returns true, or #f
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (list-sort predicate list)
returns: a list containing the elements of list sorted according to predicate
libraries: (rnrs base), (rnrs)
```

## 6.4 Numbers

Scheme中数可以分为: 整数、有理数、实数、复数; 也可以分为精确的(exact)、不精确的(inexact).

精确的整数和有理数算数支持任意精度. 尽管可能有其他表示, 不精确的数用机器硬件或系统软件支持的浮点数(floating-point numbers)表示.

Scheme中大多数数上的操作是精确性保持的(exactness preserving): 如果操作数是精确的, 则结果值是精确的; 如果操作数是不精确的或混合使用精确的操作数和不精确的操作数, 则结果值是不精确的.

例:

- 整数: `3`, `+19`, `-10000`, `208423089237489374`
- 有理数: `3/4`, `-6/5`, `1/1208203823`; `1.5`, `0.034`, `-10e-10`, `1.5e-5`
- 实数: Scheme中没有无理数的语法
- 复数: `3+4i`, `3.2-3/4i`, `+i`, `-3e-5i`; `1.1@1.764`, `-1@-1/2`
- 精确的: `1`, `#e1`, `1/1`, `#e1/1`, `#e1.0`, `#e1e0` (前缀`#e`强制转型为精确的)
- 不精确的: `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0`; `#i3/10`, `0.3`, `#i0.3`, `3e-1` (前缀`#i`强制转型为不精确的)

数默认基数是10, 特殊的前缀`#b`、`#o`、`#d`、`#x`可用于分别表示基数2、8、10、16. 例: `#b10101`、`#o72`、`#xC7`.

指数标记`s`(short)、`f`(single)、`d`(double)、`l`(long)可以出现在默认指数标记`e`的位置, 用于负载科学计数法中数的默认大小, 实现中数的默认大小至少精度为double.


语法或过程中的命名确定:

- `num`: 复数, 即所有的数;
- `real`: 实数;
- `rat`: 有理数;
- `int`: 整数;
- `exint`: 精确的整数.

``` scheme
procedure: (exact? num)
returns: #t if num is exact, #f otherwise
procedure: (inexact? num)
returns: #t if num is inexact, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (= num1 num2 num3 ...)
procedure: (< num1 num2 num3 ...)
procedure: (> num1 num2 num3 ...)
procedure: (<= num1 num2 num3 ...)
procedure: (>= num1 num2 num3 ...)
returns: #t if the relation holds, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (+ num ...)
returns: the sum of the arguments num ...
procedure: (- num)
returns: the additive inverse of num
procedure: (- num1 num2 num3 ...)
returns: the difference between num1 and the sum of num2 num3 ...
procedure: (* num ...) 
returns: the product of the arguments num ...
procedure: (/ num)
returns: the multiplicative inverse of num
procedure: (/ num1 num2 num3 ...)
returns: the result of dividing num1 by the product of num2 num3 ...
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (zero? num)
returns: #t if num is zero, #f otherwise
procedure: (positive? real)
returns: #t if real is greater than zero, #f otherwise
procedure: (negative? real)
returns: #t is real is less than zero, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (even? int)
returns: #t if int is even, #f otherwise
procedure: (odd? int)
returns: #t if int is odd, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (finite? real)
returns: #t is real is finit, #f otherwise
procedure: (infinite? real)
returns: #t if real is infinite, #f otherwise
procedure: (nan? real)
returns: #t is real is a NaN, #f otherwise
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (quotient int1 int2)
returns: the integer quotient of int1 and int2
procedure: (remainder int1 int2)
returns: the integer remainder of int1 and int2
procedure: (modulo int1 int2)
returns: the integer modulus of int1 and int2
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (div x1 x2)
procedure: (mod x1 x2)
procedure: (div-and-mod x1 x2)
procedure: (div0 x1 x2)
procedure: (mod0 x1 x2)
procedure: (div0-and-mod0 x1 x2)
returns: 
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (truncate real)
returns: the integer closest to real toward zero
procedure: (floor real)
returns: the integer closest to real toward -∞
procedure: (ceiling real)
returns: the integer closest to real toward +∞
procedure: (round real)
returns: the integer closest to readl
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (abs real)
returns: the absolute value of real
procedure: (max real1 real2 ...)
returns: the maximum of real1 real2 ...
procedure: (min real1 real2 ...)
returns: the minimum of real1 real2 ...
procedure: (gcd int ...)
returns: the greatest common divisor of its arguments int ...
procedure: (lcm int ...)
returns: the least common multiple of it arguments int ...
procedure: (expt num1 num2)
returns: num1 raised to the num2 power
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (inexact num) 
returns: an inexact representation of num
procedure: (exact num)
returns: an exact representation of num
procedure: (exact->inexact num)
returns: an inexact representation of num
procedure: (inexact->exact num)
returns: an exact representation of num
libraries: (rnrs base), (rnrs)
libraries: (rnrs r5rs)
```

``` scheme
procedure: (rationalize real1 real2)
returns: 
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (numerator rat)
returns: the numerator of rat
procedure: (denominator rat)
returns: the denominator of rat
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (real-part num)
returns: the real component of num
procedure: (imag-part num)
returns: the imaginary component of num
procedure: (make-rectangular real1 real2)
returns: a complex number with real component real1 and imaginary component real2
procedure: (make-polar real1 real2)
returns: a complex number with magnitude real1 and ange real2
procedure: (angle num)
returns: the angle part of the polar representation of num
procedure: (magnitude num)
returns: the magnitude of num
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (sqrt num)
returns: the pricipal square root of num
procedure: (exact-integer-seqt n)
returns: 
procedure: (exp num)
returns: e to the num power
procedure: (log num)
returns: the natural logarithm of num
procedure: (log num1 num2)
returns: the base-num2 logarithm of num1
procedure: (sin num)
procedure: (cos num)
procedure: (tan num)
procedure: (asin num)
procedure: (acos num)
procedure: (atan num)
procedure: (atan real1 real2)
returns: 
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (bitwise-not exint)
returns: the bitwise not of exint
procedure: (bitwise-and exint ...)
returns: the bitwise and of exint ...
procedure: (bitwise-ior exint ...)
returns: the bitwise inclusive or of exint ...
procedure: (bitwise-xor exint ...)
returns: the bitwise exclusive or of exint ...
procedure: (bitwise-if exint1 exint2 exint3)
returns: the bitwise "if" of its arguments
procedure: (bitwise-bit-count exint)
returns: 
procedure: (bitwise-length exint)
returns: 
procedure: (bitwise-first-bit-set exint)
returns: the index of the leaset significant bit set in exint
procedure: (bitwise-bit-set? exint1 exint2)
returns: #t if bit exint2 of exint1 is set, #f otherwise
procedure: (bitwise-copy-bit exint1 exint2 exint3)
returns: exint1 with bit exint2 replaced by exint3
procedure: (bitwise-bit-field exint1 ezxint2 exint3)
returns: 
procedure: (bitwise-copy-bit-field exint1 exint2 exint3 exint4)
returns: 
procedure: (bitwise-arithmetic-shift-right exint1 exint2)
returns: exint1 arithmetically shifted right by exint2 bits
procedure: (bitwise-arithmetic-shift-left exint1 exint2)
returns: exint1 shifted left by exint2 bits
procedure: (bitwise-arithmetic-shift exint1 exint2)
returns: 
procedure: (bitwise-rotate-bit-field exint1 exint2 exint3 exint4)
returns: 
procedure: (bitwise-reverse-bit-field exint1 exint2 exint3)
returns: 
libraries: (rnrs arithmetic bitwise), (rnrs)
```

``` scheme
procedure: (string->number string)
procedure: (string->number string radix)
returns: the number represented by string, or #f
procedure: (number->string num)
procedure: (number->string num radix)
procedure: (number->string num radix precision)
returns: an external representation of num as a string
libraries: (rnrs base), (rnrs)
```

## 6.5 Fixnums

**Fixnums** 表示在固定数量范围$\left [ -2^{w-1}, 2^{w-2}-1 \right ]$内的精确的整数, 固定数量宽度(fixnum width)$w$至少为24.

语法或过程中的命名确定:

- `fx`: fixnums


``` scheme
procedure: (fixnum? obj)
returns: #t if obj is a fixnum, #f otherwise
procedure: (least-fixnum)
returns: the least (most negative) fixnum supported by the implementation
procedure: (greatest-fixnum)
returns: the greatest (most positive) fixnum supported by the implementation
procedure: (fixnum-width)
returns: the implementation-dependent fixnum width
procedure: (fx=? fx1 fx2 fx3 ...)
procedure: (fx<? fx1 fx2 fx3 ...)
procedure: (fx>? fx1 fx2 fx3 ...)
procedure: (fx<=? fx1 fx2 fx3 ...)
procedure: (fx>=? fx1 fx2 fx3 ...)
returns: #t if the realtion holds, #f otherwise
procedure: (fxzero? fx)
returns: #t if fx is zero, #f otherwise
procedure: (fxpositive? fx)
returns: #t if fx is greater than zero, #f otherwise
procedure: (fxnegative? fx)
returns: #t if fx is less than zero, #f otherwise
procedure: (fxeven? fx)
returns: #t if fx is even, #f otherwise
procedure: (fxodd? fx)
returns: #t if fx is odd, #f otherwise
procedure: (fxmin fx1 fx2 ...)
returns: the minimum of fx1 fx2 ...
procedure: (fxmax fx1 fx2 ...)
returns: the maximum of fx1 fx2 ...
procedure: (fx+ fx1 fx2)
returns: the sum of fx1 and fx2
procedure: (fx- fx)
returns: the additive inverse of fx
procedure: (fx- fx1 fx2)
returns: the difference between fx1 and fx2
procedure: (fx* fx1 fx2)
returns: the product of fx1 and fx2
procedure: (fxdiv fx1 fx2)
procedure: (fxmod fx1 fx2)
procedure: (fxdiv-and-mod fx1 fx2)
returns: 
procedure: (fxdiv0 fx1 fx2)
procedure: (fxmod0 fx1 fx2)
procedure: (fxdiv0-and-mod0 fx1 fx2)
returns: 
procedure: (fx+/carry fx1 fx2 fx3)
procedure: (fx-/carry fx1 fx2 fx3)
procedure: (fx*/carry fx1 fx2 fx3)
returns: 
procedure: (fxnot fx)
returns: the bitwise not of fx
procedure: (fxand fx ...)
returns: the bitwise and of fx ...
procedure: (fxior fx ...)
returns: the bitwise inclusive or of fx ...
procedure: (fxxor fx ...)
returns: the bitwise exclusive or of fx ...
procedure: (fxif fx1 fx2 fx3)
returns: the bitwise "if" of its argument
procedure: (fxbit-count fx)
returns: 
procedure: (fxlength fx)
returns: 
procedure: (fxfirst-bit-set fx)
returns: the index of the least significant bit set in fx
procedure: (fxbit-set? fx1 fx2)
returns: #t if bit fx2 of fx1 is set, #f otherwise
procedure: (fxcopy-bit fx1 fx2 fx3)
returns: fx1 with bit fx2 replcated by fx3
procedure: (fxbit-field fx1 fx2 fx3)
returns: 
procedure: (fxcopy-bit-field fx1 fx2 fx3 fx4)
returns: 
procedure: (fxarithmetic-shift-right fx1 fx2)
returns: 
procedure: (fxarithmetic-shift-left fx1 fx2)
returns: 
procedure: (fxarithmetic-shift fx1 fx2)
returns: 
procedure: (fxrotate-bit-field fx1 fx2 fx3 fx4)
returns: 
procedure: (fxreverse-bit-field fx1 fx2 fx3)
returns: 
libraries: (rnrs arithmetic fixnums), (rnrs)
```

## 6.6 Flonums

**Flonums** 表示不精确的实数.

语法或过程中的命名确定:

- `fl`: flonums

``` scheme
procedure: (flonum? obj)
returns: #t if obj is a fixnum, #f otherwise
procedure: (fl=? fl1 fl2 fl3 ...)
procedure: (fl<? fl1 fl2 fl3 ...)
procedure: (fl>? fl1 fl2 fl3 ...)
procedure: (fl<=? fl1 fl2 fl3 ...)
procedure: (fl>=? fl1 fl2 fl3 ...)
returns: #t if the realtion holds, #f otherwise
procedure: (flzero? fl)
returns: #t if fl is zero, #f otherwise
procedure: (flpositive? fl)
returns: #t if fl is greater than zero, #f otherwise
procedure: (flnegative? fl)
returns: #t if fl is less than zero, #f otherwise
procedure: (flinteger? fl)
returns: #t if fl is integer, #f otherwise
procedure: (flfinite? fl)
returns: #t if fl is finite, #f otherwise
procedure: (flinfinite? fl)
returns: #t if fl is infinite, #f otherwise
procedure: (flnan? fl)
returns: #t if fl is a NaN, #f otherwise
procedure: (fleven? fl)
returns: #t if fl is even, #f otherwise
procedure: (flodd? fl)
returns: #t if fl is odd, #f otherwise
procedure: (flmin fl1 fl2 ...)
returns: the minimum of fl1 fl2 ...
procedure: (flmax fl1 fl2 ...)
returns: the maximum of fl1 fl2 ...
procedure: (fl+ fl ...)
returns: the sum of fl ...
procedure: (fl- fl)
returns: the additive inverse of fl
procedure: (fl- fl1 fl2 fl3 ...)
returns: the difference between fl1 and the sum of fl2 fl3 ...
procedure: (fl* fl ...)
returns: the product of the arguments fl ...
procedure: (fl/ fl)
returns: the multiplicative inverse of fl
procedure: (fl/ fl fl2 fl3 ...)
returns: the result of dividing fl1 by the product of fl2 fl3 ...
procedure: (fldiv fl1 fl2)
procedure: (flmod f1 fl2)
procedure: (fldiv-and-mod fl1 fl2)
returns: 
procedure: (fldiv0 fl1 fl2)
procedure: (flmod0 f1 fl2)
procedure: (fldiv0-and-mod0 fl1 fl2)
returns: 
procedure: (fltruncate fl)
returns: the integer closest to fl toward zero
procedure: (flfloor fl)
returns: the integer closest to fl toward -∞
procedure: (flceiling fl)
returns: the integer closest to fl toward +∞
procedure: (flround fl)
returns: the integer closest to fl
procedure: (flnumerator fl)
returns: the numerator of fl
procedure: (fldenominator fl)
returns: the denominator of fl
procedure: (flabs fl)
returns: absolute value of fl
procedure: (flexp fl)
returns: e to the fl power
procedure: (fllog fl)
returns: the natural logarithm of fl
procedure: (fllog fl1 fl2)
returns: the base-fl2 logarithm of fl1
procedure: (flsin fl)
procedure: (flcos fl)
procedure: (fltan fl)
procedure: (flasin fl)
procedure: (flacos fl)
procedure: (flatan fl)
procedure: (flatan fl1 fl2)
returns: 
procedure: (flsqrt fl)
returns: the principal square root of fl
procedure: (flexpt fl1 fl2)
returns:  fl1 raised to the fl2 power
procedure: (fixnum->flonum fx)
returns: the flonum representation closest to fx
procedure: (real->flonum real)
returns:  the flonum representation closest to real
libraries: (rnrs arithmetic flonums), (rnrs)
```

## 6.7 Characters

``` scheme
procedure: (char=? char1 char2 char3 ...)
procedure: (char<? char1 char2 char3 ...)
procedure: (char>? char1 char2 char3 ...)
procedure: (char<=? char1 char2 char3 ...)
procedure: (char>=? char1 char2 char3 ...)
procedure: (char-ci=? char1 char2 char3 ...)
procedure: (char-ci<? char1 char2 char3 ...)
procedure: (char-ci>? char1 char2 char3 ...)
procedure: (char-ci<=? char1 char2 char3 ...)
procedure: (char-ci>=? char1 char2 char3 ...)
returns: #t if the relation holds, #f otherwise
libraries: (rnrs base), (rnrs)
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (char-alphabetic? char)
returns: #t if char is a letter, #f otherwise
procedure: (char-numeric? char)
returns: #t if char is a digit, #f otherwise
procedure: (char-whitespace? char)
returns: #t if char is whitespace, #f otherwise
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (char-lower-case? char)
returns: #t if char is lower case, #f otherwise
procedure: (char-upper-case? char)
returns: #t if char is upper case, #f otherwise
procedure: (char-title-case? char)
returns: #t if char is title case, #f otherwise
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (char-general-category char)
returns: a symbol representing the Unicode general category of char
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (char-upcase char)
returns: the upper-case character counterpart of char
procedure: (char-downcase char)
returns: the lower-case character equivalent of char
procedure: (char-titlecase char)
returns: the title-case character equivalent of char
procedure: (char-foldcase char)
returns: the case-folded character equivalent of char
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (char->integer char)
returns: the Unicode scalar value of char as an exact integer
procedure: (integer->char n)
returns: the character corresponding to the Unicode scalar value n
libraries: (rnrs base), (rnrs)
```

## 6.8 Strings

``` scheme
procedure: (string=? string1 string2 string3 ...)
procedure: (string<? string1 string2 string3 ...)
procedure: (string>? string1 string2 string3 ...)
procedure: (string<=? string1 string2 string3 ...)
procedure: (string>=? string1 string2 string3 ...)
procedure: (string-ci=? string1 string2 string3 ...)
procedure: (string-ci<? string1 string2 string3 ...)
procedure: (string-ci>? string1 string2 string3 ...)
procedure: (string-ci<=? string1 string2 string3 ...)
procedure: (string-ci>=? string1 string2 string3 ...)
returns: #t if the relation holds, #f otherwise
libraries: (rnrs base), (rnrs)
libraries: (rnrs unicode), (rnrs)
```

``` scheme
procedure: (string char ...)
returns: a string containing the characters char ...
procedure: (make-string n)
procedure: (make-string n char)
returns: a string of length n
procedure: (string-length string)
returns: the number of characters in string
procedure: (string-ref string n)
returns: the nth character (zero-based) of string
libraries: (rnrs base), (rnrs)
procedure: (string-copy string)
returns: a new copy of string
procedure: (string-append string ...)
returns: a new string formed by concatenating the strings string ...
procedure: (substring string start end)
returns: a copy of string from start (inclusive) to end (exclusive)
procedure: (string->list string)
returns: a list of the characters in string
procedure: (list->string list)
returns: a string of the characters in list
```

``` scheme
procedure: (string-set! string n char)
procedure: (string-fill! string n char)
returns: unspecified
libraries: (rnrs mutable-strings), (rnrs)
```

``` scheme
procedure: (string-upcase string)
returns: the upper-case equivalent of string
procedure: (string-downcase string)
returns: the lower-case equivalent of string
procedure: (string-foldcase string)
returns: the case-folded equivalent of string
procedure: (string-titlecase string)
returns: the title-case equivalent of string
procedure: (string-normalize-nfd string)
returns: the Unicode normalized from D of string
procedure: (string-normalize-nfkd string)
returns: the Unicode normalized from KD of string
procedure: (string-normalize-nfc string)
returns: the Unicode normalized from C of string
procedure: (string-normalize-nfkc string)
returns: the Unicode normalized from KC of string
libraries: (rnrs unicode), (rnrs)
```

## 6.9 Vectors

``` scheme
procedure: (vector obj ...)
returns: a vector of the objects obj ...
procedure: (make-verctor n)
procedure: (makr-vector n object)
returns: a vector of length n
procedure: (vector-length vector)
returns: the number of elements in vector
procedure: (vector-ref vector n)
returns: the nth element (zero-based) of vector
procedure: (vector-set! vector n obj)
returns: unspecified
procedure: (vector-fill! vector obj)
returns: unspecified
procedure: (vector->list vector)
returns: a list of the elements of vector
procedure: (list->vector list)
returns: a vector of the elements of list
libraries: (rnrs base), (rnrs)
```

``` scheme
procedure: (vector-sort predicate vector)
returns: a vector containing the elements of vector, sorted according to predicate
procedure: (vector-sort! predicate vector)
returns: unspeficied
libraries: (rnrs sotring), (rnrs)
```

## 6.10 Bytevectors

``` scheme
procedure: (endianness symbol)
returns: symbol
procedure: ((native-endianness))
returns: a symbol naming the implementation's native endianness
procedure: (make-bytevector n)
procedure: (make-bytevector n fill)
returns: a new bytevector of length n
procedure: (bytevector-length bytevector)
returns: the length of bytevector in 8-bit bytes
procedure: (bytevector=? bytevector1 bytevector2)
returns: #t if the relation holds, #f otherwise
procedure: (bytevector-fill! bytevector fill)
returns: unspecified
procedure: (bytevector-copy bytevector)
returns: a new bytevector that is a copy of bytevector
procedure: (bytevector-copy! src src-start dst dst-start n)
returns: unspecified
procedure: (bytevector-u8-ref bytevector n)
returns: the 8-bit unsigned byte at index n (zero-based) of bytevector
procedure: (bytevector-s8-ref bytevector n)
returns: the 8-bit signed byte at index n (zero-based) of bytevector
procedure: (bytevector-u8-set! bytevector u8)
returns: unspecified
procedure: (bytevector-s8-set! bytevector s8)
returns: unspecified
procedure: (bytevector->u8-list bytevector)
returns: a list of the 8-bit unsigned elements of bytevector
procedure: (u8-list->bytevector list)
returns: a new bytevector of the elements of list
procedure: (bytevector-u16-native-ref bytevector n)
returns: the 16-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s16-native-ref bytevector n)
returns: the 16-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u32-native-ref bytevector n)
returns: the 32-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s32-native-ref bytevector n)
returns: the 32-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u64-native-ref bytevector n)
returns: the 64-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s64-native-ref bytevector n)
returns: the 64-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u16-native-set! bytevector n u16)
procedure: (bytevector-s16-native-set! bytevector n s16)
procedure: (bytevector-u32-native-set! bytevector n u32)
procedure: (bytevector-s32-native-set! bytevector n s32)
procedure: (bytevector-u64-native-set! bytevector n u64)
procedure: (bytevector-s64-native-set! bytevector n s64)
returns: unspecified

procedure: (bytevector-u16-ref bytevector n eness)
returns: the 16-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s16-ref bytevector n eness)
returns: the 16-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u32-ref bytevector n eness)
returns: the 32-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s32-ref bytevector n eness)
returns: the 32-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u64-ref bytevector n eness)
returns: the 64-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-s64-ref bytevector n eness)
returns: the 64-bit signed integer at index n (zero-based) of bytevector
procedure: (bytevector-u16-set! bytevector n u16 eness)
procedure: (bytevector-s16-set! bytevector n s16 eness)
procedure: (bytevector-u32-set! bytevector n u32 eness)
procedure: (bytevector-s32-set! bytevector n s32 eness)
procedure: (bytevector-u64-set! bytevector n u64 eness)
procedure: (bytevector-s64-set! bytevector n s64 eness)
returns: unspecified

procedure: (bytevector-uint-ref bytevector n eness size)
returns: the size-bit unsigned integer at index n (zero-based) of bytevector
procedure: (bytevector-sint-ref bytevector n eness size)
returns: the size-bit signed integer at index n (zero-based) of bytevector

procedure: (bytevector-uint-set! bytevector n s64 eness size)
procedure: (bytevector-sint-set! bytevector n s64 eness size)
returns: unspecified

procedure: (bytevector->uint-list bytevector eness size)
returns: a new list of the size-bit unsigned elements of bytevector
procedure: (bytevector->sint-list bytevector eness size)
returns: a new list of the size-bit signed elements of bytevector
procedure: (uint-list->bytevector list eness size)
procedure: (sint-list->bytevector list eness size)
returns: a new bytevector of the elements of list

procedure: (bytevector-ieee-single-native-ref bytevector n)
procedure: (bytevector-ieee-double-native-ref bytevector n)
returns: the single/double floating-point value at index n (zero-based) of bytevector
procedure: (bytevector-ieee-single-native-set! bytevector n x)
procedure: (bytevector-ieee-double-native-set! bytevector n x)
returns: unspecified
procedure: (bytevector-ieee-single-ref bytevector n eness)
procedure: (bytevector-ieee-double-ref bytevector n eness)
returns: the single/double floating-point value at index n (zero-based) of bytevector
procedure: (bytevector-ieee-single-set! bytevector n x eness)
procedure: (bytevector-ieee-double-set! bytevector n x eness)
returns: unspecified
libraries: (rnrs bytevectors), (rnrs)
```

## 6.11 Symbols


## 6.12 Booleans


## 6.13 Hashtables


## 6.14 Enumerations