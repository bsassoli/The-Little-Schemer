# Notes on *The Little Schemer* (April 2023)

## Chapter 1
Introduces basic Scheme syntactic entities:
- **atom**: not `null` nor a pair&mdash; you can think this as one or more chars or digits;
- **list**: either the empty list `'()` or one or more atoms or lists enclosed by parentheses
- **s-expression** : an atom or a list

Fundamentally introduces `car` and `cdr` as **selectors** and `cons` as **constructor** for lists.

**Predicates**: `null?` and `eq?`.



