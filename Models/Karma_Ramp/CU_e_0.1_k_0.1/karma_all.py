
#c=run('karma',c='karma_hom',DS=1e-2,NPR=4)
#c=run(c,IRS=6,DS='-',NTST=50,NPR=7,DSMAX=1e-2,NMX=100) #done!
#plot c



# Lets compute the "U" first
#Load the karma.f90 and c.karma
Karma = load('karma')

#Run and store first run
run1=run(Karma)
#Run backwards, and append
run1=run1 + run(Karma,DS='-')
#Relabel
run1 = relabel(run1)
#Plot bifurcation diagram
p = plot(run1)
p.config(bifurcation_y='v')
#wait()

#Look for the U
##set new start label to the first HP label
hb1=load(run1('HB1'),ISW=2)
##Continue from this label in two parameters
u = run(hb1,UZSTOP={'c':[0,10]})
u = u + run(hb1,DS='-',UZSTOP={'c':[0,10]})
p = plot(u)
p.config(bifurcation_y=['c'])



#Look for the C
#Look for the period
#IPS=2 as we're looking through period
#NMX is the number of sampled points
#NPR is the number of points listed in the output: if equal, only get last one
#SP is the listed output names - we say BP0, which will never appear, so get none shown

#hb1_period = load(run1('HB1'),IPS=2,ICP=["I",11],NMX=100000,NPR=100000,DS=0.001,DSMAX=0.1,SP=['BP0'])#,NTST=200,NCOL=4,RL0=0.1,RL1=1000)
hb1_period = load(run1('HB1'),IPS=2,ICP=["I",11],NMX=100000000,NPR=100000,DS=0.001,DSMAX=0.1,SP=['BP0'])
#Stop when the period is massive
u_period = run(hb1_period,UZSTOP={'PERIOD':1.5e5})
p=plot(u_period)
p.config(bifurcation_y="PERIOD")

#Use the end point given by UZstop of this and continue in I,C
#Only care about c between 0 and 3
homoclinic = load(u_period("UZ1"),IPS=2,ICP=["I","c"],NMX=100000,NPR=100,DS=0.00001,DSMAX=0.001)
hom = run(homoclinic,UZSTOP={'c':[0,10]})
hom = hom + run(homoclinic,DS='-',UZSTOP={'c':[0,10]})
p=plot(hom + u)
p.config(bifurcation_y='c')



hom.toArray()
hom.writeRawFilename("hom.dat")

u.toArray()
u.writeRawFilename("u_curve.dat")


clean()

