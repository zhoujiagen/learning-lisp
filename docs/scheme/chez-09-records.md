
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
# 9 Records
## 9.1 Defining Records
## 9.2 Procedural Interface
## 9.3 Inspection
