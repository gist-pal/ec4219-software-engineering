from z3 import *

x = Bool('x')
y = Bool('y')

f = And (x, y)

solve (f)
