# Lets compute the "C" first
c=run('fhn',c='fhn_hom',DS=1e-2,NPR=4)
c=run(c,IRS=6,DS='-',NTST=50,NPR=7,DSMAX=1e-2,NMX=100) #done!
# Now the U
print("Run the U")
u=run('fhn')
u=run(u('HB1'),ISW=2,ICP=[1,2],UZSTOP={'s':[1.5]})
u=run(u('UZ1'),DS='-',NMX=200,DSMAX=5e-2)
plot c+u

