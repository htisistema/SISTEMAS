@echo off
cls
clipper @insercom /n
clipper @saf /n
clipper @saf1 /n
clipper @saf2 /n
*BLINKER FI @SAF
rtlink fi saf,saf1,saf2 LIB clipper,extend,novterm,dbfcdx,gaspro52 OUT safnovo
del saf.obj
del saf1.obj
del saf2.obj

