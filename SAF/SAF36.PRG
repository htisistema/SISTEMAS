*******************************
* INCLUSAO DE FUNDO DE CAIXA
*******************************

FUNCTION saf36
***************
LOCAL MPRG:='SAF36'
LOCAL opcao,tela,lci,cci,lba,cba,i,mdata,mvalor,mtraco

*IF ! ver_nivel(mprg,'INCLUSAO FUNDO DE CAIXA','13456',nivel_acess)
*        RETURN NIL
*ENDIF

CLEAR GETS

lci = 15
cci = 21
lba = 19
cba = 79
tela = SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        botao(lci,cci,lba,cba,,' Fundo de Caixa ')
        @ lci+1,cci+1 TO lci+1,cba-1
        SET INTEN ON
        mensagem('Preencha os Campos - <ESC> p/Retornar ')
        mdata := mdata_sis
        mvalor := 0
        opcao = 'S'
        DEVPOS(lci+2,cci+2);DEVOUT('Data.......:')
        DEVPOS(lci+3,cci+2);DEVOUT('Valor R$...:')

        @ lci+2,cci+15 GET mdata
        @ lci+3,cci+15 GET mvalor PICT '99,999,999.99' VALID IF(EMPTY(mvalor),.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        mensagem('Confirma o Valor Fundo de Caixa [S/n]:')
        @ 23,COL() GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N'
                LOOP
        ENDIF

        ********************
        SELE('caix');ORDSETFOCUS(1)
        ********************

        IF ADIREG()
                REPLA tipo WITH 'FD'
                REPLA data WITH mdata
                REPLA venci WITH mdata
                REPLA documento WITH '9999999999'
                REPLA valor WITH mvalor
                REPLA descri1 WITH 'Fundo de Caixa: '+DTOC(mdata)+' - Hrs:'+TIME()
                REPLA cod_opera WITH cod_operado
                COMMIT
                UNLOCK
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ELSE
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
        FOR i = 1 TO 2
                SET DEVI TO PRINT
                SET PRINTER TO LPT1
                imprt('M','E',2)
                DEVPOS(PROW(),01);DEVOUT(memp+resa)
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm +' - '+ mfone_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+' -  C.G.C.: '+mcgc_firm)
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)

                DEVPOS(PROW()+2,25);DEVOUT('V A L E S')

                DEVPOS(PROW()+2,00);DEVOUT('Data do Vale..:')
                DEVPOS(PROW(),16);DEVOUT(mdata)
                DEVPOS(PROW(),16);DEVOUT(mdata)

                DEVPOS(PROW()+2,00);DEVOUT('Valor do Vale.:')
                DEVPOS(PROW(),16);DEVOUT(LTRIM(TRANSFORM(mvalor,'99,999,999.99')))

                DEVPOS(PROW()+2,00);DEVOUT('Descricao.....:')
                DEVPOS(PROW(),16);DEVOUT(mdescricao)
                DEVPOS(PROW()+4,00);DEVOUT(PADC('...........................           ............................',80))
                DEVPOS(PROW()+1,00);DEVOUT(PADC('         C A I X A                              Solicitante       ',80))

                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                DEVPOS(PROW()+2,00);DEVOUT(REPLI('-',80))
                DEVPOS(PROW()+1,00);DEVOUT(PADC('Corte Aqui...',80))
                DEVPOS(PROW()+7,00);DEVOUT(' ')
        NEXT
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
RETURN NIL

