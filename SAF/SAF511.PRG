************************************************
* TABELA DE MENSALIDADES
************************************************

FUNCTION saf511
***************
LOCAL mprg:='SAF511'
LOCAL opcao,mtraco,mtit,mtipo

IF ! ver_nivel(mprg,'RELATORIO TABELA DE MENSALIDADES','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N'
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        ********************************
        SELE('tabmen');ORDSETFOCUS(6)
        GO TOP
        ********************************
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        mrel := 'LPT1'
        SET DEVI TO PRINT
        SET PRINT TO 'LPT1'
        mcont_aluno := 0
        mpag := 1
        mtit := 'T A B E L A   D E   M E N S A L I D A D E'
        cabecalho(mpag,mtit)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('GST')
        DEVPOS(PROW(),06);DEVOUT('PARCELA')
        DEVPOS(PROW(),16);DEVOUT('   VALOR')
        DEVPOS(PROW(),27);DEVOUT('VENCIMENTO')
        DEVPOS(PROW(),40);DEVOUT('OPERADOR')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(tabmen->gst)
                DEVPOS(PROW(),09);DEVOUT(tabmen->parcela)
                DEVPOS(PROW(),16);DEVOUT(TRANSFORM(tabmen->valor,'9,999.99'))
                DEVPOS(PROW(),27);DEVOUT(tabmen->data_venc)
                DEVPOS(PROW(),43);DEVOUT(tabmen->operador)
                SKIP
                IF PROW() >= 58
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('GST')
                        DEVPOS(PROW(),06);DEVOUT('PARCELA')
                        DEVPOS(PROW(),16);DEVOUT('   VALOR')
                        DEVPOS(PROW(),27);DEVOUT('VENCIMENTO')
                        DEVPOS(PROW(),40);DEVOUT('OPERADOR')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        RETURN NIL
ENDDO

