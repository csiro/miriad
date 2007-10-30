	integer maxlen,maxkeys
	parameter(maxlen=10000,maxkeys=32)
	character pbuf*(maxlen),keys(maxkeys)*8
	integer lu(maxkeys),k1(maxkeys),k2(maxkeys),nkeys
	logical expanded(maxkeys),qhelp
	common/keycommc/pbuf,keys
	common/keycomm/ lu,k1,k2,expanded,nkeys,qhelp
