c  Common block for key.for
c  Last mod:    pjt	10-may-90	added argv0
c               pjt     16-mar-91       added -h option, needs qhelp
c		pjt      8-nov-91	more bufferspace
	integer maxlen,maxkeys
	parameter(maxlen=7000,maxkeys=32)
	character pbuf*(maxlen),keys(maxkeys)*8
	integer lu(maxkeys),k1(maxkeys),k2(maxkeys),nkeys
	logical expanded(maxkeys),qhelp
	common/keycommc/pbuf,keys
	common/keycomm/ lu,k1,k2,expanded,nkeys,qhelp
