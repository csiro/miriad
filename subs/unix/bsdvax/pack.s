#	.title	Pack

#	.psect	$code,rd,nowrt,pic,shr,long
	.text

# Convert from VAX reals to IEEE.

# in    *4(ap)
# out   *8(ap)
# n 	12(ap)

	.globl	_packr_c
_packr_c:
	.word	4		# .entry packr_c,^m<r2>
	movl	12(ap),r0
	moval	*4(ap),r1
	moval	*8(ap),r2
	pushl	$0
1:	divf3	$0f4.0,(r1)+,(sp)
	movb	1(sp),(r2)+
	movb	(sp),(r2)+
	movb	3(sp),(r2)+
	movb	2(sp),(r2)+
	sobgtr	r0,1b
	ret

# Convert IEEE reals to VAX reals.

	.globl	_unpackr_c
_unpackr_c:
	.word	4		# .entry unpackr_c,^m<r2>
	movl	12(ap),r0
	moval	*4(ap),r1
	moval	*8(ap),r2
	pushl	$0
1:	movb	(r1)+,1(sp)
	movb	(r1)+,(sp)
	movb	(r1)+,3(sp)
	movb	(r1)+,2(sp)
	mulf3	$0f4.0,(sp),(r2)+
	sobgtr	r0,1b
	ret

# Convert a D_floating number into an IEEE double precision.

	.globl	_packd_c
_packd_c:
	.word	12			# .entry packd_c,^m<r2,r3>
	movl	12(ap),r0
	moval	*4(ap),r1
	moval	*8(ap),r2
	clrq	-(sp)

1:	movw	(r1)+,6(sp)		# Undo clunchy word reversal ordering.
	movw	(r1)+,4(sp)
	movw	(r1)+,2(sp)
	movw	(r1)+,(sp)

	ashq	$-3,(sp),(sp)		# Shift right 3 bits.
	extzv	$4,$8,6(sp),r3		# Get the exponent.
	addw2	$894,r3			# Convert to IEEE exponent bias.
	insv	r3,$4,$11,6(sp)		# Put back IEEE exponent.

	movb	7(sp),(r2)+		# Move to output, reversing bytes.
	movb	6(sp),(r2)+
	movb	5(sp),(r2)+
	movb	4(sp),(r2)+
	movb	3(sp),(r2)+
	movb	2(sp),(r2)+
	movb	1(sp),(r2)+
	movb	(sp),(r2)+
	sobgtr	r0,1b
	ret

# Unpack an IEEE double precision to VAX double precision.

	.globl	_unpackd_c
_unpackd_c:
	.word	12
	movl	12(ap),r0
	moval	*4(ap),r1
	moval	*8(ap),r2
	clrq	-(sp)			# Spare space.

1:	movb	(r1)+,7(sp)		# Move the double to a buffer,
	movb	(r1)+,6(sp)		#  performing byte reversal on the way.
	movb	(r1)+,5(sp)
	movb	(r1)+,4(sp)
	movb	(r1)+,3(sp)
	movb	(r1)+,2(sp)
	movb	(r1)+,1(sp)
	movb	(r1)+,(sp)
	extzv	$4,$11,6(sp),r3		# Get the exponent.
	subw2	$894,r3			# Convert exponent.
	bbc	$15,6(sp),2f		# Branch if positive number.
	bisw2	$256,r3			# Set the sign bit for neg. numbers.
2:	insv	r3,$4,$9,6(sp)		# Insert the exponent.
	ashq	$3,(sp),(sp)		# Shift to account for different
					#   exponent sizes.
	movw	6(sp),(r2)+		# Move from buffer to output, doing
	movw	4(sp),(r2)+		#   clunchy word reversal.
	movw	2(sp),(r2)+
	movw	(sp),(r2)+
	sobgtr	r0,1b
	ret


# Routines which convert from or to FITS integer format. I.e. do
# some byte flipping.

#  Register Usage:
#  r0	Counter
#  (r1)	in(i)
#  (r2)	Out(i)

	.globl	_unpack16_c
_unpack16_c:
	.word	4			# .entry unpack16_c,^m<iv,r2>
	moval	*8(ap),r2
	movaw	*4(ap),r1
	movl	12(ap),r0
	pushl	$0			#  Space on the stack.
1:	movb	(r1)+,1(sp)
	movb	(r1)+,(sp)
	cvtwl	(sp),(r2)+
	sobgtr	r0,1b
	ret

	.globl	_pack16_c
_pack16_c:
	.word	4			# .entry pack16_c,^m<iv,r2>
	moval	*4(ap),r1
	movaw	*8(ap),r2
	movl	12(ap),r0
	pushl	$0			#  Space on the stack.
1:	movl	(r1)+,(sp)
# 	cvtlw	(r1)+,(sp)
	movb	1(sp),(r2)+
	movb	(sp),(r2)+
	sobgtr	r0,1b
	ret

# Pack64 and unpack64 routines not implemented.

	.globl	_unpack64_c
	.globl	_pack64_c
_unpack64_c:
_pack64_c:
        .word   0
	halt
	ret

	.globl	_unpack32_c
	.globl	_pack32_c
_unpack32_c:
_pack32_c:
	.word	4			# .entry pack32_c,^m<r2>
	movl	12(ap),r0
	moval	*4(ap),r1
	moval	*8(ap),r2
	pushl	$0
1:	movl	(r1)+,(sp)
	movb	3(sp),(r2)+
	movb	2(sp),(r2)+
	movb	1(sp),(r2)+
	movb	(sp),(r2)+
	sobgtr	r0,1b
	ret
