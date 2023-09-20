@ECHO OFF
CLS
ECHO OFF
@echo off
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siacbema /n
RTLINK /FREE FI siacbema,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52,relogio

