@ECHO OFF
CLS
F:
CD\SAF\1997
COPY *.DBF C:\BACKUP\1997
CD\SAF\1998
COPY *.DBF C:\BACKUP\1998
CD\SAF
CLS
ARJ A -V1440000 -R SAFBKP *.DBF *.FPT
if not exist \saf\safbkp.arj goto fim
CLS
ECHO Insira o Disco 1 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.arj A:
ECHO .
if not exist \saf\safbkp.a01 goto fim
ECHO Insira o Disco 2 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a01 A:
ECHO .
if not exist \saf\safbkp.a02 goto fim
ECHO Insira o Disco 3 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a02 A:
ECHO .
if not exist \saf\safbkp.a03 goto fim
ECHO Insira o Disco 4 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a03 A:
ECHO .
if not exist \saf\safbkp.a04 goto fim
ECHO Insira o Disco 5 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a04 A:
ECHO .
if not exist \saf\safbkp.a05 goto fim
ECHO Insira o Disco 6 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a05 A:
ECHO .
if not exist \saf\safbkp.a06 goto fim
ECHO Insira o Disco 7 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a06 A:
ECHO .
if not exist \saf\safbkp.a07 goto fim
ECHO Insira o Disco 8 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a07 A:
ECHO .
if not exist \saf\safbkp.a08 goto fim
ECHO Insira o Disco 9 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a08 A:
ECHO .
if not exist \saf\safbkp.a09 goto fim
ECHO Insira o Disco 10 para fazer o BACKUP
ECHO .
PAUSE
COPY safbkp.a09 A:


:fim
CLS
DEL SAFBKP.A*
ECHO Fim do BACKUP

