	integer port,BufSize,MaxItems
	parameter(port=5001,BufSize=2048,MaxItems=32)
	integer DEFINE,DISPLAY,CLEAR,CHECK,WAIT,SET
	parameter(DEFINE=1,DISPLAY=2,CLEAR=3,CHECK=4,WAIT=5,SET=6)
	integer BUTTONS,SLIDERS,CURSORS,STATUSES
	parameter(BUTTONS=1,SLIDERS=2,CURSORS=3,STATUSES=4)
c
	integer Handle,NItems,BufLen,Buffer(BufSize),IOBuf(BufSize)
	character Items(MaxItems)*8
	common/ctrlcomm/Handle,NItems,BufLen,Buffer,IOBuf
	common/ctrlcomc/Items
