************************************************
* ETIQUETAS DO COLEGIO
************************************************

FUNCTION saf5223
***************
LOCAL mprg:='SAF5223'
LOCAL lci,cci,lba,cba,tela,mquantd,opcao,i

IF ! ver_nivel(mprg,'RELATORIO DE ETIQUETAS DO COLEGIO','125')
        RETURN NIL
ENDIF

mensagem('Digite a quantidade de etiquetas:')
mquantd := 0
@ 23,COL()+1 GET mquantd PICT '99999'
READ
IF LASTKEY() = 27
        RETURN NIL
ENDIF
mensagem('Confirma a impressao de '+STRZERO(mquantd,5)+' ETIQUETAS [S/N]:')
opcao := 'S'
@ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
READ
IF LASTKEY() = 27
        RETURN NIL
ENDIF
IF opcao = 'N'
        RETURN NIL
ENDIF
SET DEVI TO PRINT
* * SET PRINT TO LPT1
FOR i = 1 TO mquantd
                imprt('M','N')
                DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
                DEVPOS(PROW()+1,00);DEVOUT('AVENIDA OLIVEIRA LIMA, No. 824')
                DEVPOS(PROW()+1,00);DEVOUT('BOA VISTA - RECIFE - PE')
                DEVPOS(PROW()+1,00);DEVOUT('CEP: 50050-390')
                imprt('M','N',3)
NEXT
SETPRC(00,00)
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
RETURN NIL

