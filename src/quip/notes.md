# Notes

# Environment

* why is there a inner and outer environment
    to protect global variables from those defined in the lambda.

* a lamdba function is:
- an anonimous function
- it has a body, (which is an expression)
- it has arguments, whis is a list of symbols.
- calling a lamdba
 * eval provided args
 * store them in environment (inner)
 * evaluate the body using the new environment.

# Eval

* Eval takes not just an expression and returns a result (reduction), it also can update the environment (state).
* Eval : Environment -> Expression -> Result Error (Expression, Environment)

```lisp 
(begin (define map (lambda (f xs) (if xs (cons (f (car xs)) (map f (cdr xs))) nil))) (map (lambda (x) (* x x)) (list 1 2 3)))
```