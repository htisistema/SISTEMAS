**************************************
* ATUALIZACAO DE CONTRATO
**************************************
FUNCTION saf_atu(mdir)

LOCAL matricula,mcont:=0

REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
SET DELE ON
SET DATE BRITI
CLEAR ALL
@ 00,00 CLEAR TO 24,79
USE cdmensal  alias cdmen EXCLUSIVE new
USE cadaluno  alias aluno EXCLUSIVE new
*****************
SELE('cdmen');ORDSETFOCUS(4)
*****************
GO TOP
DEVPOS(2,00);DEVOUT('Lendo arquivo CADALUNO           Registro No.:')
WHILE ! EOF()
        DEVPOS(2,48);DEVOUT(mcont)
        IF cdmen->desc_bolsa < 0
                cdmen->desc_bolsa := cdmen->desc_bolsa * -1
                mcont++
        ENDIF
        SKIP
ENDDO
DBUNLOCKALL()
DBCOMMITALL()
RETURN NIL
