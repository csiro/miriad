	integer MAXDATA,MAXPOL
	parameter(MAXDATA=5000,MAXPOL=2)
c
	complex Vis(MAXDATA),Model(MAXDATA)
	integer b1(MAXDATA),b2(MAXDATA),t1(MAXDATA),t2(MAXDATA)
	integer zerovar(MAXPOL+1),nzero
	real angfreq(MAXDATA)
	common/mfcalcom/vis,model,angfreq,b1,b2,t1,t2,zerovar,nzero
