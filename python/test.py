# Issue: https://github.com/pearu/f2py/issues/49
# Therefore no f2py implementation possible
import random 

rand = random.random()
print(rand)

result = (10 + 990*random.random())
print (result)

conversion = 1E-3
print(conversion)

i = result*conversion
print(i)
