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

z = curry(lambda x:(
    (
        "yes"
    ) if (
        x
    ) else (
        "no"
    )
))
p = Print(z(True))
main = unit