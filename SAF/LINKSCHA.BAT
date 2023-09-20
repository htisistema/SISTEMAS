@ECHO OFF
CLS
ECHO OFF
@echo off
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siacscha /n
RTLINK /FREE FI siacscha,insercom,amdk6 LIB clib.lib,cl_ecf.lib,c_ecf_l.lib,clipper,extend,dbfcdx,novterm,GASPRO52,relogio

