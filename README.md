# cs225Final

Make sure you have ocaml 4.06.0, and ppx_deriving installed.

    > opam switch 4.06.0
    > opam install ppx_deriving

There are two special files in this directory which help merlin and ocamlbuild
know how to build the project.

First, there is a hidden file .merlin which instructs merlin to:
1. find source files in this directory
2. find build files in the _build directory
3. use the ppx_deriving.std package when checking files

Next, there is a special file _tags which instructs ocamlbuild to:
1. use the ppx_deriving.std package when compiling files

There is an included Makefile that will build all *.ml files in the current
directory as the default target:

    > make

If you want to execute a file, for example, hw3.ml, just execute:

    > make final

Running either of these commands will generate a bunch of compiled files. These
will get placed in the _build directory, and that directory will be created
automatically if it doesn't already exist. To get rid of the _build directory,
just execute:

    > make clean


For the final assignment of CS225, I used assignment 3 as the final code that I am going to write Exception for. Until now, the exceptions for "Syntax for extended simply typed lambda calculus expressions, types and values" is what I am working on. (P.S.the code of assignment 3 is a bunch of code based on the Simply Typed Lambda Calculus.)

In the code, at line 124, two types of parameter are belong to type_env and expression. Type of type_env is (string * ty) list.Thus, the exception catch and handling should check the if e0 belongs to expression. If not, raise error and handling the exception.

First, at line 112, I added a new exception type named EXP_ERROR. Then at line 283-306, expressions that functions should return has been checked. Finally, EXP_ERROR has been added to every functions.


