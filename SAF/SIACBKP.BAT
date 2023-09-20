@ECHO OFF
CLS
CD\SIAC
DEL SIACBKP.A*
COPY *.DBF C:\SIACBKP
COPY *.FPT C:\SIACBKP
COPY *.DBF \\COLORCEN\C\SIACBKP
COPY *.FPT \\COLORCEN\C\SIACBKP
ARJ A -Y -V1440000  SIACBKP *.DBF *.FPT
if not exist \siac\siacbkp.arj goto fim
CLS
ECHO Insira o Disco 1 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.arj A:
ECHO .
if not exist \siac\siacbkp.a01 goto fim
ECHO Insira o Disco 2 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a01 A:
ECHO .
if not exist \siac\siacbkp.a02 goto fim
ECHO Insira o Disco 3 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a02 A:
ECHO .
if not exist \siac\siacbkp.a03 goto fim
ECHO Insira o Disco 4 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a03 A:
ECHO .
if not exist \siac\siacbkp.a04 goto fim
ECHO Insira o Disco 5 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a04 A:
ECHO .
if not exist \siac\siacbkp.a05 goto fim
ECHO Insira o Disco 6 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a05 A:
ECHO .
if not exist \siac\siacbkp.a06 goto fim
ECHO Insira o Disco 7 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a06 A:
ECHO .
if not exist \siac\siacbkp.a07 goto fim
ECHO Insira o Disco 8 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a07 A:
ECHO .
if not exist \siac\siacbkp.a08 goto fim
ECHO Insira o Disco 9 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a08 A:
ECHO .
if not exist \siac\siacbkp.a09 goto fim
ECHO Insira o Disco 10 para fazer o BACKUP
ECHO .
PAUSE
COPY siacbkp.a09 A:


:fim
CLS
CD\SIAC
DEL SIACBKP.A*
ECHO Fim do BACKUP

