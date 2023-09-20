*******************************
* RETIRADA DE DEPOSITOS
*******************************

FUNCTION saf37
*****************
LOCAL MPRG:='SAF37'
LOCAL opcao,tela,lci,cci,lba,cba,i,mdata,mvalor,mdescri1,mdescri2,mtraco

IF ! ver_nivel(mprg,'RETIRADA P/DEPOSITO DO CAIXA','13456',nivel_acess)
        RETURN NIL
ENDIF
CLEAR GETS

lci = 15
cci = 21
lba = 21
cba = 79
tela = SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        botao(lci,cci,lba,cba,,' Retirada p/Depositos ')
        SET INTEN ON
        mensagem('Preencha os Campos - <ESC> p/Retornar ')
        mdata := mdata_sis
        mvalor := 0
        mdescri1 := SPACE(40)
        mdescri2 := SPACE(40)
        opcao = 'S'
        DEVPOS(lci+2,cci+2);DEVOUT('Data.......:')
        DEVPOS(lci+3,cci+2);DEVOUT('Valor R$...:')
        DEVPOS(lci+4,cci+2);DEVOUT('Descricao..:')

        @ lci+2,cci+15 GET mdata
        @ lci+3,cci+15 GET mvalor PICT '99,999,999.99' VALID IF(EMPTY(mvalor),.F.,.T.)
        @ lci+4,cci+15 GET mdescri1 PICT '@!'
        @ lci+5,cci+15 GET mdescri2 PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        mensagem('Confirma o Retirada p/Depositos [S/n]:')
        opcao := 'S'
        @ 23,COL() GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF

        ********************
        SELE('caix');ORDSETFOCUS(1)
        ********************

        IF ADIREG()
                REPLA tipo WITH 'DP'
                REPLA data WITH mdata
                REPLA venci WITH mdata
                REPLA documento WITH '9999999999'
                REPLA valor WITH mvalor * -1
                REPLA descri1 WITH mdescri1
                REPLA descri2 WITH mdescri2
                REPLA cod_opera WITH cod_operado
                COMMIT
                UNLOCK
        ELSE
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
                mtipo_aut := 0
                mtela_aut := SAVESCREEN(00,00,24,79)
                botao(10,35,12,67,,' Opcao de Autenticacoes ')
                @ 11,36 PROMPT ' Autenticar '
                @ 11,50 PROMPT ' Imprimir Recibo '
                SET INTEN ON
                MENU TO mtipo_aut
                IF  mtipo_aut = 2
                        RESTSCREEN(00,00,24,79,mtela_aut)
                        mtela_aut := SAVESCREEN(00,00,24,79)
                        WHILE LASTKEY() <> 27
                                botao(10,10,12,60,,' IMPRESSAO DE RECIBO ')
                                DEVPOS(11,11);DEVOUT('Tecle <ENTER> p/Imprimir - <ESC> p/abandonar')
                                INKEY(0)
                                IF LASTKEY() = 27;EXIT;ENDIF
                                mtraco := REPLI('=',60)
                                SET DEVI TO PRINT
                                SET PRINTER TO LPT1
                                imprt('M','N')
                                DEVPOS(PROW(),01);DEVOUT(memp+resa)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm+  +'Fone: '+mfone_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)

                                imprt('M','N')
                                DEVPOS(PROW()+2,00);DEVOUT(PADC('RETIRADA PARA DEPOSITO',35))
                                imprt('M','C')

                                DEVPOS(PROW()+2,00);DEVOUT('Data da Retirada :')
                                DEVPOS(PROW(),18);DEVOUT(mdata)

                                DEVPOS(PROW()+2,00);DEVOUT('Valor da Retirada:')
                                DEVPOS(PROW(),18);DEVOUT(LTRIM(TRANSFORM(mvalor,'99,999,999.99')))

                                DEVPOS(PROW()+2,00);DEVOUT('Descricao........:')
                                DEVPOS(PROW(),18);DEVOUT(mdescri1)
                                DEVPOS(PROW()+1,18);DEVOUT(mdescri2)

                                DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('         C A I X A         ',60))
                                DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('        Solicitante        ',60))

                                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                                IF 1 > 1 .AND. i < 1
                                        DEVPOS(PROW()+3,00);DEVOUT(' ')
                                ENDIF
                                DEVPOS(PROW()+15,00);DEVOUT(' ')
                                SETPRC(00,00)
                                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        ENDDO
                        RESTSCREEN(00,00,24,79,mtela_aut)
                ELSE
                        RESTSCREEN(00,00,24,79,mtela_aut)
                        mtela_aut := SAVESCREEN(00,00,24,79)
                        WHILE LASTKEY() <> 27
                                botao(10,10,13,60,,' AUTENTICACAO DE DOCUMENTO ')
                                DEVPOS(11,11);DEVOUT('Insira o Documento na impressora p/autenticar')
                                DEVPOS(12,11);DEVOUT('  <ENTER> p/Autenticar - <ESC> p/abandonar')
                                INKEY(0)
                                IF LASTKEY() = 27;EXIT;ENDIF
                                SET DEVI TO PRINT
                                SET PRINT TO LPT1
                                imprt('M','C')
                                DEVPOS(00,00);DEVOUT('RETDEP'+LEFT(ALLTRIM(mdescri1),20)+cod_operado+DTOC(mdata)+TRANSFORM(mvalor,'999999.99'))
                                imprt('M','N',1)
                                SETPRC(00,00)
                                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        ENDDO
                        RESTSCREEN(00,00,24,79,mtela_aut)
                ENDIF
*       ENDIF
ENDDO
RETURN NIL

