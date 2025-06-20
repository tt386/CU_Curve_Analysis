# Lets compute the "C" first
c=run('morrislecar',c='ml_hom',DS=1e-2,NPR=16)
c=run(c,IRS=6,DS='-',NTST=50,NPR=7,DSMAX=1e-2,NMX=100) #done!

plot c
