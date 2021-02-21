from inspect import signature

def curry(func):
    num_params = len(signature(func).parameters)
    def rec(args):
        if len(args) == num_params:
            return func(*args)
        return lambda x: rec(args + [x])
    return rec([])

def let(inn=None,**kwargs):
    return inn(**kwargs)

def fix(f):
    return lambda *args: f(fix(f))(*args)

unit = "{}"
Mul = lambda x: lambda y: x * y
Add = lambda x: lambda y: x + y
Sub = lambda x: lambda y: x - y
Neg = lambda x: -x
Pos = lambda x: x
EqInt = lambda x: lambda y: x == y
Int2Str = str
Print = print

fib_1 = fix(lambda fib: lambda x:(
    (
        0
    ) if (
        EqInt(x)(0)
    ) else (
        (
            1
        ) if (
            EqInt(x)(1)
        ) else (
            Add(fib(Sub(x)(1)))(fib(Sub(x)(2)))
        )
    )
))
main = fib_1(8)