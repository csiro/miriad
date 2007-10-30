#!/bin/csh -f

#  Drive the data quality observation scripts.

#  The data must be initially read using atlod, with the command
#    atlod in=?? out=dq.uv options=reweight,birdie,xycorr

dqo_init

dqo_onaxis

dqo_offaxis

dqo_report > report.txt

rm -rf junk* *.stats
