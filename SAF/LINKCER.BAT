@ECHO OFF
CLS
ECHO.
CHOICE /C:SN Deseja Compilar o SIAC
IF ERRORLEVEL 2 GOTO OPSIACVEND
IF ERRORLEVEL 1 GOTO SIAC
:SIAC
@echo off
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siaccer /n
clipper @sac1 /n
clipper @sac2 /n
clipper @sac3 /n
clipper @sac4 /n
clipper @sacnfcer /n
rtlink fi siaccer,sac1,sac2,sac3,sac4,sacnfcer,insercom LIB clipper,extend,dbfcdx,novterm,GASPRO52
GOTO END
:END
CLS
ECHO Fim da Compilacao












