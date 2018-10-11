This is an interpreter for a simple functional language designed for the "Programming Languages and Paradigms" third year obligatory course of my Computer Science studies on University of Warsaw that I took in 2014.

The solution uses BNFC to generate a parser. The definition of the language grammar
is contained in the [bnfc/haml.cf](./bnfc/haml.cf) file.

The [good/program](./good/program) file contains an example of a correct program. The [bad](./bad) directory contains a few examples of incorrect programs.

The interpreter is based on the Computation monad defined as follows:
```
newtype Computation a = Computation { runComputation :: ReaderT Env (ErrorT String IO) a }
```
where Env represents an enviroment containing the values required to evaluate an expression, which can grow in subexpressions. IO is used to print the values of top-level expressions.

What is interesting is the type of Env:
```
type Env = M.Map Name (Computation Value)
```
which means that the definition of Computation is cyclic. This allows to implement recursion in a simple way: during the interpretation of an expression representing a function definition, the current environment is first extended to contain a computation returning our function, then saved in the value representing the function. Thanks to this the function can access itself when being called.

Basic definitions (types and the initial environment) is contained in the file [Computation.hs](./Computation.hs), and the interpreter -- a set of functions mapping parsed expressions to Computations -- is contained in [Interpreter.hs](./Interpreter.hs). The [Run.hs](./Run.hs) file loads the input and passes it to the interpreter.

