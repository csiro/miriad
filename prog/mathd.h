	integer OFFI,OFFR,OFFD,SIZEI,SIZER,SIZED,VALI,VALR,VALD
	parameter(OFFI=4, OFFR=4, OFFD=8)
	parameter(SIZEI=4,SIZER=4,SIZED=8)
	parameter(VALI=2, VALR=4, VALD=5)
	integer CONSTANT,VECTOR,ERROR,MAXITEMS,MAXLEN
	parameter(CONSTANT=1,VECTOR=3,ERROR=0)
	parameter(MAXITEMS=10,MAXLEN=1024)
	character typo*1,types(MAXITEMS)*1
	integer tIn,size,Items(MAXITEMS),nItems
	integer Bufi(MAXLEN)
	double precision Bufd(MAXLEN)
	common/MathdCom/Bufd,Bufi,tIn,size,nItems,Items
	common/MathdCmC/types,typo
