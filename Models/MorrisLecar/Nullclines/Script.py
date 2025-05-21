# -*- coding: utf-8 -*-
"""
Created on Mon Nov 18 15:04:22 2024

@author: tt386
"""


import numpy as np
import matplotlib.pyplot as plt
from scipy import optimize


V1 = -1.2
V2 = 18
V3 = -0  #VRIED PARAMETER: 0, -13, -21
V4 = 10#30
phi = 0.15#0.04
Vk = -100#-84
VL = -70#-60
VCa = 50#120
gk = 20#8
gL = 2
gCa = 20#4.4
C = 2#20

I = 12.42#18.38

"""
gL/=C
gCa/=C
gk/=C
#I /=C
#phi/=200
"""

def MSS(V):
    return 1/2 * (1 + np.tanh((V-V1)/V2))

def NSS(V):
    return 0.5 * (1 + np.tanh((V-V3)/V4))

def tN(V):
    return 1/(phi * np.cosh((V-V3)/(2*V4)))


V = np.linspace(-100,100,10000)





def Null_V(V):
    return -1/(gk * (V-Vk)) * (-I + gL*(V-VL) + gCa*MSS(V)*(V-VCa))

def Null_N(V):
    return NSS(V)



def DiffFunc(V):
    return Null_V(V) - Null_N(V)


root = optimize.newton(DiffFunc,-10)
print("V =",root)
print(Null_N(root))
print(Null_V(root))




plt.figure()

NN = Null_N(V)
plt.plot(V,NN,color='k')

for I in [2*15,2*18.38,2*20]:
    
    NV = Null_V(V)
    plt.plot(V,NV,label='p=%0.3f'%(I))

plt.ylim(0,0.25)
plt.xlim(-90,50)
plt.legend()


plt.ylabel("w")
plt.xlabel("u")

#plt.ylim(-1,2)
#plt.xlim(-100,100)
plt.savefig("NullclineComparisons.png")
