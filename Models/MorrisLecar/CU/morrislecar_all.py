import numpy as np
import matplotlib.pyplot as plt


#c=run('karma',c='karma_hom',DS=1e-2,NPR=4)
#c=run(c,IRS=6,DS='-',NTST=50,NPR=7,DSMAX=1e-2,NMX=100) #done!
#plot c



# Lets compute the "U" first
#Load the karma.f90 and c.karma
morrislecar = load('morrislecar')

run1=run(morrislecar,UZSTOP={"I":[-20,40]},NMX=10000000,NPR=1000000)
#Run backwards, and append
run1_back= run(morrislecar,DS='-',UZSTOP={"I":[-20,40]},NMX=10000000,NPR=1000000)
#Relabel
run1 = merge(run1 + run1_back)#relabel(run1)
#Plot bifurcation diagram
p = plot(run1)
p.config(bifurcation_y='v')
#wait()
p.config(stability=True)

"""
r2 = load('karma',"c"=1)
print(r2)
run2 = run(r2)
run2 = run2 + run(r2,DS='-')
p=plot(run2)
p.config(bifurcation_y='y')
p.config(stability=True)
wait()
"""


print("Look for the U")
##set new start label to the first HP label
hb1=load(run1('HB1'),ISW=2,NMX=100000,NPR=10000)#,DS=0.001,DSMAX=0.1)
##Continue from this label in two parameters
u = run(hb1,UZSTOP={'c':[-10,10],'I':[-60,40]})
print("And the other way")
u = u + run(hb1,DS='-',UZSTOP={'c':[-10,10],'I':[-60,40]})
p = plot(u)
p.config(bifurcation_y=['c'])
p.config(stability=True)
#p.config(top_title='U curve c=%0.2f'%(c))


"""
#ZH
zh1 = load(u("ZH1"),ISW=2,NMX=1000000,NPR=100000,DS=0.001,DSMAX=0.1)
zh = run(zh1,UZSTOP={'c':[-10,10]})
zh = zh + run(zh1,UZSTOP={'c':[-10,10]},DS='-')
p = plot(zh)
p.config(bifurcation_y=['c'])
p.config(stability=True)
#p.config(top_title='ZH c=%0.2f'%(c))
"""


print("Start period hunt")
hb1_period = load(run1('HB1'),IPS=2,ICP=["I",11],NMX=1000000000,NPR=100000000,DS=0.001,DSMAX=0.1,SP=['BP0'])#,NTST=200,NCOL=4,RL0=0.1,RL1=1000)
#Stop when the period is massive

u_period = run(hb1_period,UZSTOP={"PERIOD":[-10,30000]})#{'PERIOD':[0,10000]})#UZSTOP={'PERIOD':1000})
#u_period = u_period + run(hb1_period,UZSTOP={"PERIOD":[0,10000]},DS='-')

p=plot(u_period + run1)
p.config(stability=True)
#p.config(bifurcation_y=['PERIOD'])
p.config(bifurcation_y=['MAX v'])
#p.config(top_title='c=%0.2f'%(c))
#p.savefig("Saved/c=%0.2f.png"%(c))
#p.savefig("Saved/IMG_" + str(i).zfill(2) + '.png')
plt.close()


#Use the end point given by UZstop of this and continue in I,C
#Only care about c between 0 and 3
#homoclinic = load(u_period("UZ1"),IPS=2,ICP=["I","c"],NMX=1000000,NPR=100,DS=0.00001,DSMAX=0.001,NTST=600,NCOL=6)
homoclinic = load(u_period("UZ1"),IPS=2,ICP=["I","c"],NMX=1000000,NPR=10000,DS=0.00001,DSMAX=0.001,NTST=600,NCOL=6) #For less points labelled

hom = run(homoclinic,UZSTOP={'c':[-10,10],'I':[-30,40]})
hom = hom + run(homoclinic,DS='-',UZSTOP={'c':[-1,3],'I':[-30,40]})
p=plot(hom + u)
p.config(bifurcation_y='c')
p.config(stability=True)


hom.toArray()
hom.writeRawFilename("hom.dat")

u.toArray()
u.writeRawFilename("u_curve.dat")


clean()

