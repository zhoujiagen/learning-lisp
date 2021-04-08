# Chez Scheme

|#|Title|Progress|Description|
|:---|:---|:---|:---|
|1|Introduction|100%|20210327|
|2|Getting Started|100%|20210328|
|3|Going Further|100%|20210401|
|4|Procedures and Variable Bindings|100%|20210407|
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

- 1 Introduction   
- 2 Getting Started   
- 3 Going Further

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

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
    <c3_syntactic_extension> syntactic extension\l
    | more recursion: letrec, named let, tail recursion\l
    | <c3_continuations> continutation\l
    | <c3_internal_definition> internal definition\l
    | <c3_library> library\l
    "]
    c3 -> c3_concepts;
    
    c3_syntactic_extension [shape=record, label="
    core syntactic form\l
    | form, definition, expression, application\l
    | begin\l
    | define-syntax, syntax-rules\l
    | let*\l
    "]
    c3_concepts:c3_syntactic_extension -> c3_syntactic_extension;
    
    c3_continuations [shape=record, label="
    call/cc\l
    | continuation passing styles\l
    "]
    c3_concepts:c3_continuations -> c3_continuations;
    
    c3_internal_definition [shape=record, label="
    letrec*\l
    | case\l
    "]
    c3_concepts:c3_internal_definition -> c3_internal_definition;

    c3_library [shape=record, label="
    library\l
    | export\l
    | import\l
    "]
    c3_concepts:c3_library -> c3_library;

    root -> {c1 c2 c3} [style=invis]
}
%}
</div>

- 4 Procedures and Variable Bindings

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    #root [style=invis]

    #c4 [label="Procedures and Variable Bindings"];
    c4_concepts [shape=record, label="
    variable reference\l
    | <c4_lambdas> lambda
    | <c4_local_binding> local binding
    | <c4_multiple_values> multiple values
    | <c4_variable_definition> variable definition\l
    | <c4_assignments> assignment\l
    "]
    #c4 -> c4_concepts;

    c4_lambdas [shape=record, label="
    (lambda formals body1 body2 ...)\l
    | (case-lambda clause ...)\l
    | do, substring\l
    "]
    c4_concepts:c4_lambdas -> c4_lambdas;

    c4_local_binding [shape=record, label="
    (let ((var expr) ...) body1 body2 ...)\l
    | (let* ((var expr) ...) body1 body2 ...)\l
    | (letrec ((var expr) ...) body1 body2 ...)\l
    | (letrec* ((var expr) ...) body1 body2 ...)\l
    "]
    c4_concepts:c4_local_binding -> c4_local_binding;

    c4_multiple_values [shape=record, label="
    (let-values ((formals expr) ...) body1 body2 ...)\l
    | (let*-values ((formals expr) ...) body1 body2 ...)\l
    | values\l
    "]
    c4_concepts:c4_multiple_values -> c4_multiple_values;

    c4_variable_definition [shape=record, label="
    (define var expr)\l
    | (define var)\l
    | (define (var0 var1 ...) body1 body2 ...)\l
    | (define (var0 . varr) body1 body2 ...)\l
    | (define (var0 var1 var2 ... . varr) body1 body2 ...)\l
    "]
    c4_concepts:c4_variable_definition -> c4_variable_definition;

    c4_assignments [shape=record, label="
    (set! var expr)\l
    "]
    c4_concepts:c4_assignments -> c4_assignments;
 
    #root -> {c4} [style=invis]
}
%}
</div>

- 5 Control Operations

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    #root [style=invis]
    
    #c5 [label="Control Operations"];
    c5_concepts [shape=record, label="
    <c5_procedure_application> procedure application\l
    | <c5_sequencing> sequencing\l
    | <c5_conditionals> conditionals\l
    | <c5_recursion_iteration> recursion, iteration\l
    | <c5_map_fold> mapping, folding\l
    | <c5_continuation> continuation\l
    | <c5_delayed_eval> delayed evaluation\l
    | <c5_multiple_values> multiple values\l
    | <c5_eval> eval\l
    "]
    #c5 -> c5_concepts;

    c5_procedure_application [shape=record, label="
    (expr0 expr1 ...)\l
    | (apply procedure obj ... list)\l
    "]
    c5_concepts:c5_procedure_application -> c5_procedure_application;

    c5_sequencing [shape=record, label="
    (begin expr1 expr2 ...)\l
    "]
    c5_concepts:c5_sequencing -> c5_sequencing;

    c5_conditionals [shape=record, label="
    (if test consequent alternative)\l
    | (if test consequent)\l
    | (not obj)\l
    | (and expr ...)\l
    | (or expr ...)\l
    | (cond clause1 clause2 ...)\l
    | else, =\>\l
    | (when test-expr expr1 expr2 ...)\l
    | (unless test-expr expr1 expr2 ...)\l
    | (case expr0 clause1 clause2 ...)\l
    "]
    c5_concepts:c5_conditionals -> c5_conditionals;

    c5_recursion_iteration [shape=record, label="
    (let name ((var expr) ...) body1 body2 ...)\l
    | (do ((var init update) ...) (test result ...) expr ...)\l
    "]
    c5_concepts:c5_recursion_iteration -> c5_recursion_iteration;

    c5_map_fold [shape=record, label="
    (map procedure list1 list2 ...)\l
    | (for-each procedure list1 list2 ...)\l
    | (exists procedure list1 list2 ...)\l
    | (for-all procedure list1 list2 ...)\l
    | (fold-left procedure obj list1 list2 ...)\l
    | (fold-right procedure obj list1 list2 ...)\l
    | <c5_map_fold_special_type> vector, string\l
    "]
    c5_concepts:c5_map_fold -> c5_map_fold;

    c5_map_fold_special_type [shape=record, label="
    (vector-map procedure vector1 vector1 ...)\l
    | (vector-for-each procedure vector1 vector2 ...)\l
    | (string-for-each procedure string1 string2 ...)\l
    "]
    c5_map_fold:c5_map_fold_special_type -> c5_map_fold_special_type;

    c5_continuation [shape=record, label="
    (call/cc procedure), (call-with-current-continuation procedure)\l
    | (dynamic-wind in body out)\l
    "]
    c5_concepts:c5_continuation -> c5_continuation;

    c5_delayed_eval [shape=record, label="
    (delay expr)\l
    | (force promise)\l
    "]
    c5_concepts:c5_delayed_eval -> c5_delayed_eval;

    c5_multiple_values [shape=record, label="
    (values obj ...)\l
    | (call-with-values producer consumer)\l
    "]
    c5_concepts:c5_multiple_values -> c5_multiple_values;

    c5_eval [shape=record, label="
    (eval obj environment)\l
    | (environment import-spec ...)\l
    | (null-environment version)\l
    | (scheme-report-environment version)\l
    "]
    c5_concepts:c5_eval -> c5_eval;

    #root -> {c5} [style=invis]
}
%}
</div>

- 6 Operations on Objects

Constants and Quotation:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];

    c6_concepts1 [shape=record, label="
        <c6_constants> constant\l
        | <c6_quotations> quotation\l
        "]
    
        c6_constants [shape=record, label="
        number, boolean, character, string, bytevector\l
        | immutable\l
        "]
        c6_concepts1:c6_constants -> c6_constants;
    
        c6_quotations [shape=record, label="
        quote\l
        | quasiquote\l
        | unquote\l
        | unquote-splicing\l
        "]
        c6_concepts1:c6_quotations -> c6_quotations;

}
%}
</div>  

Generic Equivalence and Type Predicates:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];

    c6_concepts2 [shape=record, label="
        <c6_generic_equivalence> generic equivalence\l
        | <c6_type_predicates> type predicate\l
        "]
    
        c6_generic_equivalence [shape=record, label="
        eq?\l
        | eqv?\l
        | equal?\l
        "]
        c6_concepts2:c6_generic_equivalence -> c6_generic_equivalence;
    
        c6_type_predicates [shape=record, label="
        boolean?\l
        | null?\l
        | pair?\l
        | number?, complex?, real?, rational?, integer?\l
        | real-values?, rational-valued?, integer-valued?\l
        | char?\l
        | string?\l
        | vector?\l
        | symbol?\l
        | procedure?\l
        | bytevector?\l
        | hashtable?\l
        "]
        c6_concepts2:c6_type_predicates -> c6_type_predicates;
}
%}
</div>

Lists and Pairs:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];

    c6_concepts3 [shape=record, label="
        <c6_list_pair> list, pair\l
        "]
    
        c6_list_pair [shape=record, label="
        {cons | car | cdr | set-car! | set-cdr!}
        | {caar | cadr | ... | cddddr}
        | {list | cons* | list? | length | list-ref | list-tail | append | reverse}
        | {memq, memv, member | memp}
        | {remq, remv, remove | remp}
        | filter\l
        | partition\l
        | find\l
        | {assq | assv | assoc |assp}
        | list-sort\l
        "]
        c6_concepts3:c6_list_pair -> c6_list_pair; 
}
%}
</div> 

Numbers:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
        
    c6_numbers [shape=record, label="
    {exact? | inexact? | exact | inexact | exact-\>inexact | inexact-\>exact}
    | {= | \< | \> | \<= | \>=}
    | {+ | - | * | /}
    | {zero? | positive? | negative? | even? | odd?}
    | {finite? | infinite? | nan?}
    | {quotient | remainder | modulo}
    | {div | mod | div-and-mod | div0 | mod0 | div0-and-mod0}
    | {truncate | floor | ceiling | round}
    | {abs | max | min | gcd | lcm | expt | sqrt | exact-integer-sqrt}
    | {exp | log | sin | cos | tan | asin | acos | atan}
    | {rationalize | numerator | denominator}
    | {real-part | imag-part | make-rectangular | make-polar | angle | magnitude}
    | <c6_numbers_bitwise> bitwise procedures\l
    | {string-\>number | number-\>string}
    "]

    c6_numbers_bitwise [shape=record, label="
    {bitwise-not | bitwise-and | bitwise-ior | bitwise-xor }
    | bitwise-if\l
    | bitwise-bit-count\l
    | bitwise-length\l
    | bitwise-first-bit-set\l
    | bitwise-bit-set?\l
    | bitwise-copy-bit\l
    | bitwise-bit-field\l
    | bitwise-copy-bit-field\l
    | bitwise-arithmetic-shift-right\l
    | bitwise-arithmetic-shift-left\l
    | bitwise-arithmetic-shift\l
    | bitwise-rotate-bit-field\l
    | bitwise-reverse-bit-field\l
    "]
    c6_numbers:c6_numbers_bitwise -> c6_numbers_bitwise;
}
%}
</div>

Fixnums:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];

    c6_fixnums [shape=record, label="
    {fixnum? | least-fixnum | greatest-fixnum | fixnum-width}
    | {fx=? | fx\<? | fx\>? | fx\<=? | fx\>? }
    | {fxzero? | fxpositive? | fxnegative?}
    | {fxeven? | fxodd?}
    | {fxmin | fxmax}
    | {fx- | fx*}
    | {fxdiv | fxmod | fxdiv-and-mod | fxdiv0 | fxmod0 | fxdiv0-and-mod0}
    | {fx+/carry | fx-/carry | fx*/carry}
    | {fxnot | fxand | fxior | fxxor}
    | {fxif | fxbit-count | fxlength}
    | {fxfirst-bit-set | fxbit-set? | fxcopy-bit | fxbit-field | fxcopy-bit-field}
    | {fxarithmetic-shift-right | fxarithmetic-shift-left | fxarithmetic-shift}
    | {fxrotate-bit-field | fxreverse-bit-field}
    "]
}
%}
</div>

Flonums:

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];

    c6_flonums [shape=record, label="
        flonum?\l
        | {fl=? | fl\<? | fl\>? | fl\<=? | fl\>=?}
        | {flzero? | flpositive? | flnegative?}
        | {flfinite? | flinfinite? | flnan?}
        | {fleven? | flodd? | flmin | flmax}
        | {fl+ | fl- | fl* | fl/}
        | {fldiv | flmod | fldiv-and-mod | fldiv0 | flmod0 | fldiv0-and-mod0}
        | {flround | fltruncate | flfloor | flceiling}
        | {flnumerator | fldenominator}
        | {flabs | flexp | fllog | flsin | flcos | fltan | flasin | flacos | flatan | flsqrt | flexpt}
        | {fixnum-\>flonum | real-\>flonum}
        "]
}
%}
</div>

Characters
Strings
Vectors
Bytevectors
Symbols
Booleans
Hashtables
Enumerations

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    c6_concepts4 [shape=record, label="
    <c6_characters> character\l
    | <c6_strings> string\l
    | <c6_vectors> vector\l
    | <c6_bytevectors> bytevector\l
    | <c6_symbols> symbol\l
    | <c6_booleans> boolean\l
    | <c6_hashtables> hashtable\l
    | <c6_enumerations> enumeration\l
    "]

    c6_characters [shape=record, label="
    {char=? | char\<? | char\>? | char\<=? | char\>?}
    | {char-ci=? | char-ci\<? | char-ci\>? | char-ci\<=? | char-ci\>?}
    | {char-alphabetic? | char-numeric? | char-whitespace?}
    | {char-lower-case? | char-upper-case? | char-title-case?}
    | {char-general-category | char-upcase | char-downcase | char-titlecase | char-foldcase}
    | {char-\>integer | integer-\>char}
    "]
    c6_concepts4:c6_characters -> c6_characters; 
    
    c6_strings [shape=record, label="
    {string=? | string\<? | string\>? | string\<=? | string\>?}
    | {string-ci=? | string-ci\<? | string-ci\>? | string-ci\<=? | string-ci\>?}
    | {string | make-string | string-length | string-ref | string-set!}
    | {string-copy | string-append | substring | string-fill!}
    | {string-upcase | string-downcase | string-foldcase |string-titlecase}
    | {string-normalize-nfd | string-normalize-nfkd | string-normalize-nfc | string-normalize-nfkc}
    | {string-\>list | list-\>string}
    "]
    c6_concepts4:c6_strings -> c6_strings; 
    
    c6_vectors [shape=record, label="
    {vector | make-vector}
    | {vector-length | vector-ref | vector-set! | vector-fill!}
    | {vector-\>list | list-\>vector}
    | {vector-sort | vector-sort!}
    "]
    c6_concepts4:c6_vectors -> c6_vectors; 
    
    c6_bytevectors [shape=record, label="
    {endianness | native-endianness}
    | make-bytevector\l
    | bytevector-length\l
    | bytevector=?\l
    | {bytevector-fill! | bytevector-copy | bytevector-copy!}
    | {bytevector-u8-ref | bytevector-s8-ref | bytevector-u8-set! | bytevector-s8-set!}
    | {bytevector-\>u8-list | u8-list-\>bytevector}
    | bytevector-xxx-native-ref: u16, s16, u32, s32, u64, s64\l
    | bytevector-xxx-native-set!: u16, s16, u32, s32, u64, s64\l
    | bytevector-xxx-ref: u16, s16, u32, s32, u64, s64\l
    | bytevector-xxx-set!: u16, s16, u32, s32, u64, s64\l
    | {bytevector-uint-ref | bytevector-sint-ref}
    | {bytevector-uint-set! | bytevector-sint-set!}
    | {bytevector-\>uint-list | bytevector-\>sint-list}
    | {uint-list-\>bytevector | sint-list-\>bytevector}
    | bytevector-ieee-xxx-native-ref: single , double\l
    | bytevector-ieee-xxx-native-set!: single , double\l
    | bytevector-ieee-xxx-ref: single , double\l
    | bytevector-ieee-xxx-set!: single , double\l
    "]
    c6_concepts4:c6_bytevectors -> c6_bytevectors; 
    
    c6_symbols [shape=record, label="
    symbol=?\l
    | {string-\>symbol | symbol-\>string}
    "]
    c6_concepts4:c6_symbols -> c6_symbols; 
    
    c6_booleans [shape=record, label="
    boolean=?\l
    "]
    c6_concepts4:c6_booleans -> c6_booleans; 
    
    c6_hashtables [shape=record, label="
    make-eq-hashtable\l
    | make-eqv-hashtable\l
    | make-hashtable\l
    | hashtable-mutable?\l
    | hashtable-hash-function\l
    | hashtable-equivalence-function\l
    | {equal-hash | string-hash | string-ci-hash | symbol-hash}
    | {hashtable-set! | hashtable-ref | hashtable-contains | hashtable-update! | hashtable-delete!}
    | {hashtable-size | hashtable-copy | hashtable-clear!}
    | {hashtable-keys | hashtable-entries}
    "]
    c6_concepts4:c6_hashtables -> c6_hashtables; 
    
    c6_enumerations [shape=record, label="
    define-enumeration\l
    | make-enumeration\l
    | {enum-set-constructor | enum-set-universe}
	| enum-set-\>list\l
	| {enum-set-subset? | enum-set=?}
	| {enum-set-member? | enum-set-union | enum-set-intersection | enum-set-difference | enum-set-complement}
	| {enum-set-projection | enum-set-indexer}
    "]
    c6_concepts4:c6_enumerations -> c6_enumerations; 
}
%}
</div>


- 7 Input and Output

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    c7 [label="Input and Output"];
    
    root -> {c7} [style=invis]
}
%}
</div>


- 8 Syntactic Extension

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    #root [style=invis]
    
    #c8 [label="Syntactic Extension"];
    c8_concepts [shape=record, label="
    <keywords> keyword\l
    | <transfomers> transfomer\l
    | <expanders> expander\l
    "]    
    #c8 -> c8_concepts;
    
    keywords [shape=record, label="
    define-syntax\l
    | let-syntax\l
    | letrec-syntax\l
    "]
    c8_concepts:keywords -> keywords;

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

    #root -> {c8} [style=invis]

}
%}
</div>


- 9 Records

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    
    c9 [label="Records"];
    
    root -> {c9} [style=invis]
}
%}
</div>


- 10 Libraries and Top-Level Programs

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    
    c10 [label="Libraries and Top-Level Programs"];
    
    root -> {c10} [style=invis]
}
%}
</div>

- 11 Exceptions and Conditions

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    
    c11 [label="Exceptions and Conditions"];
    
    root -> {c11} [style=invis]
}
%}
</div>

- 12 Extended Examples

<div>
{% dot tspl.svg
digraph tspl {
    rankdir=LR;
    splines=polyline

    node [shape=tab, width=1, height=0.1];
    edge [];
    
    root [style=invis]
    
    c12 [label="Extended Examples"];
    
    root -> {c12} [style=invis]
}
%}
</div>

## 总结

<!-- 概要记录书籍中如何解决关键性问题的. -->

## 应用

<!-- 记录如何使用书籍中方法论解决你自己的问题. -->

## 文献引用

<!-- 记录相关的和进一步阅读资料: 文献、网页链接等. -->

R. Kent Dybvig. The Scheme Programming Language. The MIT Press, 2009.

## 其他备注