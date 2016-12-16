# horname

[![Build Status](https://travis-ci.com/cocreature/horname.svg?token=k5tv9VjJ9pj7ynzqQyjR&branch=master)](https://travis-ci.com/cocreature/horname)

Parses `define-fun` clauses output by SMT solvers
and renames the variables according to annotations of the form

```
; :annot (INV_MAIN_42 i j n i j)
```

in the original SMT. The SMT-LIB standard itself does not allow naming
arguments in a function declaration.
