import numpy as np
import matplotlib.pyplot as plt 

def etha_air(T):
    offset = 1.338/100000
    gradient = 0.968/100000/100
    etha = gradient*(T)+offset
    return etha

array = []
values_nu = []
values_nu_real = [1.169/100000, 1.252/100000, 1.338/100000, 1.426/100000, 1.516/100000, 1.608/100000, 1.702/100000, 1.798/100000, 1.896/100000, 1.995/100000, 2.097/100000, 2.201/100000, 2.306/100000]
#values_etha_real = [1.63/100000, 1.68/100000, 1.729/100000, 1.778/100000, 1.825/100000, 1.872/100000, 1.918/100000, 1.963/100000, 2.008/100000, 2.052/100000, 2.096/100000, 2.139/100000, 2.181/100000]

for T in range(-20, 110, 10): 
    array.append(T)
    values_nu.append(etha_air(T))

plt.plot(array, values_nu)
plt.plot(array, values_nu_real)
plt.show()