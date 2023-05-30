#Instructions

install the [nix package manager](https://nixos.org/nix/)

run `nix develop`

this will throw you into a reproducible shell with all the required tools to run PULER programs

compile Main.hs with `ghc -O2 Main.hs`

then run `./Main repl` to start a PULER repl

this is an example output:
```
-----INPUT-----
fact = fix f \x ->
              if x == 0
              then 1
              else let nextFac = x * (f (x - 1));
                       p = (Print (Int2Str nextFac))
                   in nextFac;
main = let g = (fact 8) in {};
-----PARSED-----
fact = fix f \x -> if ((EqInt x) 0)
                   then 1
                   else let nextFac = ((Mul x) (f ((Sub x) 1)));
                            p = (Print (Int2Str nextFac));
                        in nextFac;
main = let g = (fact 8);
       in {};
-----RENAMED-----
fact = fix f \x -> if ((EqInt x) 0)
                   then 1
                   else let nextFac = ((Mul x) (f ((Sub x) 1)));
                            p = (Print (Int2Str nextFac));
                        in nextFac;
main = let g = (fact 8);
       in {};
-----INFERRED-----
fact = fix f \x -> if ((EqInt x: Int): Int -> Bool 0: Int): Bool
                   then 1: Int
                   else let nextFac = ((Mul x: Int): Int -> Int (f: Int -> Int ((Sub x: Int): Int -> Int 1: Int): Int): Int): Int;
                            p = (Print (Int2Str nextFac: Int): String): {};
                        in nextFac: Int: Int: Int: Int -> Int;
main = let g = (fact: Int -> Int 8: Int): Int;
       in {}: {}: {};: {}
-----CHECKED-----
Typechecks!
-----EVALUATED-----
1
2
6
24
120
720
5040
40320
{}
-----EMITTED-----
fact = fix(lambda f: lambda x:(
    (
        1
    ) if (
        EqInt(x)(0)
    ) else (
        let(
          nextFac = Mul(x)(f(Sub(x)(1))),
        inn = lambda nextFac:(
          let(
            p = Print(Int2Str(nextFac)),
          inn = lambda p:(
            nextFac
          ))
        ))
    )
))
main = let(
  g = fact(8),
inn = lambda g:(
  unit
))
```

Notable differences in syntax:
application is called by having parentheses around the expr ex: (a b c d)

Core.hs --> code that is shared accross all steps this includes every language construct generically defined and the main ast
Parser.hs --> parsing code for every construct written as generically as I could write it (but now I now that first lexing into tokens and then parsing those tokens would be a better plan + I also know how to structure parsing better next time)
Renamer.hs -> renaming specific code which takes care of scoping, shadowed variables are handled monoidally as in when i join (x1, x1) i'll get x2
Evaluator.hs -> interprets code taken from the parsed output and generates a value
TypeSystem.hs -> A full implementation of Hindley-Milner
Compiler.hs -> combines all the steps and is supposed to generate the instructions to be emitted
Emmitter.hs -> emmits the compiled program to python
Repl.hs -> repl

all in all the most interesting aspect of this project for me was using recursion schemes. It took me a while to wrap my head around it but now I see it as a really powerful tool.
