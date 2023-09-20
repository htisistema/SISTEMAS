@ECHO OFF
CLS
ECHO OFF
CHOICE /C:SNA Deseja Compilar o SIAC
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPSIACVEND
IF ERRORLEVEL 1 GOTO SIAC
:SIAC
@echo off
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siac /n
clipper @sac1 /n
clipper @sac2 /n
clipper @sac3 /n
clipper @sac4 /n
clipper @sacnota /n
*BLINKER @SIAC.LNK
RTLINK /FREE FI siac,sac1,sac2,sac3,sac4,sacnota,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52,relogio
clipper @sacdbf /n
RTLINK /FREE FI sacdbf,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
clipper @sacindex /n
RTLINK /FREE FI sacindex,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
GOTO OPSIACVEND
:OPSIACVEND
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar SIACVEND
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPVENDA
IF ERRORLEVEL 1 GOTO SIACVEND
:SIACVEND
ECHO.
ECHO.
@echo off
cls
clipper @insercom /n
clipper @siacvend /n
clipper @siacven1 /n
RTLINK /FREE FI siacvend,siacven1,sacnota,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
GOTO OPVENDA
:OPVENDA
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar VENDA
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPCAIXA
IF ERRORLEVEL 1 GOTO VENDA
:VENDA
@echo off
cls
clipper @insercom /n
clipper @venda /n
clipper @venda1 /n
clipper @sacnota /n
RTLINK /FREE FI venda,venda1,sacnota,insercom,amdk6 LIB clipper,extend,novterm,dbfcdx,gaspro52,relogio
del %1.obj
GOTO OPCAIXA
:OPCAIXA
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar CAIXA
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPSACDBF
IF ERRORLEVEL 1 GOTO CAIXA

:CAIXA
@echo off
cls
clipper @insercom /n
clipper @caixa /n
clipper @caixa1 /n
clipper @sacnota /n
RTLINK /FREE FI caixa,caixa1,sacnota,insercom,amdk6 LIB clipper,extend,novterm,dbfcdx,gaspro52
GOTO OPSACDBF

:OPSACDBF
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar SAC_DBF
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPMANUTEN
IF ERRORLEVEL 1 GOTO SACDBF

:SACDBF
@echo off
cls
clipper @sac_dbf /n
RTLINK /FREE FI sac_dbf,insercom,amdk6 LIB clipper,extend,novterm,dbfcdx,gaspro52
GOTO OPMANUTEN

:OPMANUTEN
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar MANUTENCAO
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO OPCFG
IF ERRORLEVEL 1 GOTO SACMANU

:SACMANU
@echo off
cls
clipper @manuten /n
RTLINK /FREE FI manuten,insercom,amdk6 LIB clipper,extend,novterm,dbfcdx,gaspro52
clipper @sacdbf /n
RTLINK /FREE FI sacdbf,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
clipper @sacindex /n
RTLINK /FREE FI sacindex,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
GOTO OPCFG

:OPCFG
CLS
ECHO.
ECHO.
CHOICE /C:SNA Deseja Compilar o SACCFG.
IF ERRORLEVEL 3 GOTO END
IF ERRORLEVEL 2 GOTO END
IF ERRORLEVEL 1 GOTO SACCFG

:SACCFG
@echo off
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @SACCFG /n
rtlink fi SACCFG,insercom,amdk6 LIB clipper,extend,novterm,dbfcdx,gaspro52
clipper @sacindex /n
RTLINK /FREE FI sacindex,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52
clipper @saccalc /n
RTLINK /FREE FI saccalc,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52 OUT siacc
del SACCALC.obj
del SACCFG.obj
GOTO END

:END
CLS
ECHO Fim da Compilacao












