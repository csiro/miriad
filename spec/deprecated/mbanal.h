	integer MAXCHAN,MAXANT,MAXBASE
	parameter(MAXCHAN=1025,MAXANT=13,MAXBASE=(MAXANT*(MAXANT+1))/2)
	integer MAXVAR,MAXSLOT,MAXIC
	parameter(MAXVAR=32,MAXSLOT=3200,MAXIC=320)
	complex corr(MAXCHAN,MAXIC)
	character var(MAXVAR)*16
	integer slotidx(MAXBASE,MAXVAR),nvar,nslot,slotav(MAXSLOT),nchan
	integer slotic(MAXSLOT),nic
c
	common/mbcom/ corr,slotidx,slotic,nvar,nslot,slotav,nchan,nic
	common/mbcomc/var

