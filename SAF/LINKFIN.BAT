ECHO OFF
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siacfin /n
clipper @sac1fin /n
clipper @sac2fin /n
RTLINK /FREE FI siacfin,sac1fin,sac2fin,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52,relogio

