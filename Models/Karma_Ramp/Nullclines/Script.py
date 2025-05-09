# -*- coding: utf-8 -*-
"""
Created on Fri Nov 22 17:01:04 2024

@author: thoma
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy import optimize

def f(u,v,us,m,d):
    return 2*(us-v**M)*(u**2-d*u**3)#y * (y-0.1)*(1-y)  + 0


def H(x,k):
    return x/(1 + np.exp(-2*x/k))
    

I = 0.087179#Numerical determination of the bifurcation
c = 2.1208#Numerical determination of the bifurcation

#From paper
us = 1.5
M=4
d=0.25
e=0.01
nb=0.5
D=1
k_small=0.0001


#u = np.linspace(0.15,0.2,10000000)#(0.178,0.18,10000000)
u = np.linspace(0.12,1.5,10000000)

def w_Nullcline(u):
    return H(u-1,k_small)/nb

def v_Nullcline(u):
    return (us + (I-u)/(2*(u**2-d*u**3)))**(1/M)


def Difference(u):
    return V_Nullcline(u) - W_Nullcline(u)



plt.figure()

for I in [0.08,0.087179,0.09]:
    """
    eq = optimize.newton(Difference,1)
    print("y=",eq,"V=",V_Nullcline(eq))
    """
    plt.plot(u,v_Nullcline(u),label='p=%0.3f'%(I))
    plt.plot(u,w_Nullcline(u),color='k')

plt.legend()
plt.ylabel("w")
plt.xlabel("u")
plt.ylim(0,1)
plt.grid()
plt.savefig("NullclineComparisons.png")
