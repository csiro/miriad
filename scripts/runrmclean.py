#!/usr/bin/env python

# RUNRMCLEAN is a python script to enable the easy use of miriad task rmclean

# What it does
# 1. Reorder the input cubes mode=312
# 2. Run the rmclean program with the specified inputs
# 3. Reorder the output cubes with mode=231
# 4. Produce P clean and model cubes, which rmclean does not do

# How to use it
# For help, 'runrmclean.py -h'

# Written 23 november 2007 by ghh
# as a companion to miriad task rmclean, version 1.0 23-Nov-2007
# Updated 05 december 2007 by ghh
# to reflect update of rmclean from v1.0 to v1.1 (add METHOD).
# Updated 10 july 2008 by ghh
# because v1.3 now outputs residual cubes as well (RESQ,RESU).
# Updated 29 March 2013 by ghh
# to include the derot and weight options

import optparse
import os
import os.path
import time

def main():

	# Produce a helpful option set
	p = optparse.OptionParser()
	p.add_option('--inq','-q',default="Q",
		help="Name of dirty Q cube [Q]")
	p.add_option('--inu','-u',default="U",
		help="Name of dirty U cube [U]")
	p.add_option('--freq','-f',default="../frequencies-Q.txt",
		help="Name of frequency file [../frequencies-Q.txt]")
        p.add_option('--weight','-t',default="",
                help="Name of weights file [none = equal weights]")
	p.add_option('--nmax','-n',default="1000",
		help="Maximum iterations [1000]")
	p.add_option('--gain','-g',default="0.1",
		help="Gain factor [0.1]")
	p.add_option('--cut','-c',default="0.01",
		help="CLEAN cutoff [0.01]")
	p.add_option('--fwhm','-w',default="0",
		help="FWHM of restoring beam [auto]")
	p.add_option('--out','-o',default=".",
		help="Output directory [.]")
	p.add_option('--meth','-m',default="xcorr",
		help="Peak finding method [xcorr]")
	p.add_option('--derot','-d',default=False,
		help='Derotate pol vectors? [default False]',
		action='store_true')
	p.add_option('--debug','-b',default=False,
		help='Debug? [default False]',
		action='store_true')
	options,arguments = p.parse_args()

	# Check for the output directory...
	if os.path.isdir(options.out) is False:
		print 'Output directory ('+options.out+') does not exist!'
		return None

	# Start the timer
	t=time
	starttime=t.time()

	# Clean up first in case some stray temp files are laying around
	print 'Initializing...'
	stat=os.system('rm -rf temp_reoq temp_reou temp_outq temp_outu'
		+' temp_modq temp_modu '+options.out+'/Qclean '+options.out
		+'/Uclean '+options.out+'/Qmodel '+options.out+'/Umodel '
		+options.out+'/niters '+options.out+'/Pclean '+options.out
		+'/Pmodel '+options.out+'/Qresid '+options.out+'/Uresid '
		+options.out+'/Presid temp_resq temp_resu')

	# First reorder the input cubes
	print 'Reordering Q cube...'
	stat=os.system('reorder in='+options.inq+' out=temp_reoq mode=312')
	if stat != 0:
		print 'Could not reorder the Q cube'
		return None
	print 'Reordering U cube...'
	stat=os.system('reorder in='+options.inu+' out=temp_reou mode=312')
	if stat != 0:
		print 'Could not reorder the U cube'
		return None

	# Next run rmclean with the user-specified (or default) options
	print 'Running RMCLEAN ...'
        if options.weight == "":
		wtpart = ""
	else:
		wtpart = " weight="+options.weight
	if options.debug:
		wtpart += " debug=1"
	stat=os.system('rmclean inq=temp_reoq inu=temp_reou freq='+options.freq
		+' nmax='+options.nmax+' gain='+options.gain+' cutoff='
		+options.cut+' fwhm='+options.fwhm+' outq=temp_outq'+wtpart
		+' outu=temp_outu modq=temp_modq modu=temp_modu ni='
		+options.out+'/niters method='+options.meth+' resq='
		+'temp_resq resu=temp_resu derot=%d'%(int(options.derot)))
	if stat != 0:
		print 'Unsuccessful run of RMCLEAN.'
		return None

	# Reorder the output cubes back into regular order
	print 'Reordering Q clean cube...'
	stat=os.system('reorder in=temp_outq out='+options.out+'/Qclean'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder Q clean cube'
		return None
	print 'Reordering U clean cube...'
	stat=os.system('reorder in=temp_outu out='+options.out+'/Uclean'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder U clean cube'
		return None
	print 'Reordering Q model cube...'
	stat=os.system('reorder in=temp_modq out='+options.out+'/Qmodel'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder Q model cube'
		return None
	print 'Reordering U model cube...'
	stat=os.system('reorder in=temp_modu out='+options.out+'/Umodel'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder U model cube'
		return None
	print 'Reordering Q residual cube...'
	stat=os.system('reorder in=temp_resq out='+options.out+'/Qresid'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder Q residual cube'
		return None
	print 'Reordering U residual cube...'
	stat=os.system('reorder in=temp_resu out='+options.out+'/Uresid'
		+' mode=231')
	if stat != 0:
		print 'Could not reorder U residual cube'
		return None

	# Finally produce P cubes
	print 'Computing P clean, residual, and model cubes...'
	stat=os.system('maths "exp=sqrt(<'+options.out+'/Qclean>**2+<'
		+options.out+'/Uclean>**2)" out='+options.out+'/Pclean')
	if stat != 0:
		print 'Could not calculate P clean cube'
		return None
	stat=os.system('maths "exp=sqrt(<'+options.out+'/Qmodel>**2+<'
		+options.out+'/Umodel>**2)" out='+options.out+'/Pmodel')
	if stat != 0:
		print 'Could not calculate P model cube'
		return None
	stat=os.system('maths "exp=sqrt(<'+options.out+'/Qresid>**2+<'
		+options.out+'/Uresid>**2)" out='+options.out+'/Presid')
	if stat != 0:
		print 'Could not calculate P residual cube'
		return None

	# Clean up temp sets
	print 'Cleaning up'
	stat=os.system('rm -rf temp_reoq temp_reou temp_outq temp_outu'
		+' temp_modq temp_modu temp_resq temp_resu')
	if stat != 0:
		print 'WARNING: Scratch sets may not have been deleted'

	# Stop the timer and report the time to run
	endtime=t.time()
	print 'RUNRMCLEAN took',(endtime-starttime)/60.0,'minutes to run'

if __name__ == '__main__':
	main()
