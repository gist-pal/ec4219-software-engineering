from z3 import *

x = Bool('x')
y = Bool('y')

f = Implies (x, y)

solve (f)
