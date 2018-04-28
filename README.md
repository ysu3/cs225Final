# cs225Final

For the final assignment of CS225, I used assignment 3 as the final code that I am going to write Exception for. Until now, the exceptions for "Syntax for extended simply typed lambda calculus expressions, types and values" is what I am working on. (P.S.the code of assignment 3 is a bunch of code based on the Simply Typed Lambda Calculus.)

In the code, at line 124, two types of parameter are belong to type_env and expression. Type of type_env is (string * ty) list.Thus, the exception catch and handling should check the if e0 belongs to expression. If not, raise error and handling the exception.

First, at line 112, I added a new exception type named EXP_ERROR. Then at line 283-306, expressions that functions should return has been checked. Finally, EXP_ERROR has been added to every functions.

However, the final.ml cannot be ran. If I had more time, I would work on how to fix this problem and write other exceptions.
