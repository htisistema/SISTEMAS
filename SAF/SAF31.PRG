*******************************
* RETIRADA DE DESPESAS DIARIAS
*******************************

FUNCTION saf31_1
*****************
LOCAL mprg:='SAF31_1'
LOCAL opcao,tela,lci,cci,lba,cba,i,mdata,mvalor,mtraco,mtp_desp

PRIVATE mdescri1,mdescri2,mcoddesp,mdocumento:=SPACE(10)

IF ! ver_nivel(mprg,'RETIRADA DE DESPESAS','1356',nivel_acess)
        RETURN NIL
ENDIF
CLEAR GETS

lci = 14
cci = 21
lba = 22
cba = 79
tela = SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
IF ! AbriArq('safcadde','desp');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 TO lci+1,cba-1
        SET INTEN ON
        mensagem('Preencha os Campos - <ESC> p/Retornar ')
        mdata := mdata_sis
        mvalor := 0
        mcoddesp := 0
        mdescri1 := SPACE(40)
        mdescri2 := SPACE(40)
        mtp_desp := SPACE(1)
        opcao = 'S'
        janela(lci,cba,' Retirada de Despesas ')
        DEVPOS(lci+2,cci+2);DEVOUT('Data........:')
        DEVPOS(lci+2,cci+28);DEVOUT('No.Documento:')
        DEVPOS(lci+3,cci+2);DEVOUT('Valor R$....:')
        DEVPOS(lci+4,cci+2);DEVOUT('Tipo Despesa:')
        DEVPOS(lci+5,cci+2);DEVOUT('Cod.Despesa.:')
        DEVPOS(lci+6,cci+2);DEVOUT('Descricao...:')

        @ lci+2,cci+16 GET mdata WHEN men_get(lci+3,cci+29,'Informe a data da DESPESA - <ESC>p/abandonar') VALID lim_get()
        @ lci+2,cci+42 GET mdocumento VALID IF(EMPTY(mdocumento),.F.,.T.)
        @ lci+3,cci+16 GET mvalor PICT '99,999,999.99' WHEN men_get(lci+4,cci+29,'Informe o valor da DESPESA - <ESC>p/abandonar') VALID IF(EMPTY(mvalor),.F.,.T.) .AND. lim_get()
        @ lci+4,cci+16 GET mtp_desp PICT '@!' WHEN men_get(lci+5,cci+17,'Tipo de despesas <A>,<B>,<C> ou deixe em branco - <ESC>p/abandonar') VALID lim_get() .AND. mtp_desp $ 'A,B,C, '
        @ lci+5,cci+16 GET mcoddesp PICT '999' WHEN men_get(lci+6,cci+29,'Informe o Codigo da DESPESA ou deixe em branco p/descricao - <ESC>p/abandonar') VALID lim_get().AND. ver_desp(mcoddesp)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        ver_desp(mcoddesp)
        @ lci+6,cci+15 GET mdescri1 PICT '@!'
        @ lci+7,cci+15 GET mdescri2 PICT '@!'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Confirma a Despesas [S/n]:')
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
                caix-> tipo := 'DE'
                caix-> nota := ' DESPESA'
                caix-> data := mdata
                caix-> venci := mdata
                caix-> documento := mdocumento
                caix-> valor := mvalor * -1
                caix-> coddesp := STRZERO(mcoddesp,3)
                caix-> descri1 := mdescri1
                caix-> descri2 := mdescri2
                caix-> cod_opera := cod_operado
                caix-> tp_desp := mtp_desp
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
                        mtraco := REPLI('=',54)
                        SET DEVI TO PRINT
                        SET PRINTER TO LPT1
                        imprt('M','C')
                        DEVPOS(PROW(),01);DEVOUT(memp+resa)
                        DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm +' - '+ mfone_firm)
                        DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+' -  C.G.C.: '+mcgc_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)

                        DEVPOS(PROW()+2,00);DEVOUT(PADC('D E S P E S A S',35))

                        DEVPOS(PROW()+2,00);DEVOUT('Data da Despesa:')
                        DEVPOS(PROW(),17);DEVOUT(mdata)

                        DEVPOS(PROW()+2,00);DEVOUT('Valor Despesa..:')
                        DEVPOS(PROW(),17);DEVOUT(LTRIM(TRANSFORM(mvalor,'99,999,999.99')))

                        IF ! EMPTY(mcoddesp)
                                DEVPOS(PROW()+2,00);DEVOUT('Descricao'+' ('+STRZERO(mcoddesp,3)+'):')
                        ELSE
                                DEVPOS(PROW()+2,00);DEVOUT('Descricao......:')
                        ENDIF
                        DEVPOS(PROW(),17);DEVOUT(mdescri1)
                        DEVPOS(PROW()+1,17);DEVOUT(mdescri2)

                        DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('         C A I X A         ',60))
                        DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('         Solicitante       ',60))

                        DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+15,00);DEVOUT(' ')
                        imprt('M','N')
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
                        DEVPOS(00,00);DEVOUT('DESP'+ALLTRIM(mdocumento)+LEFT(mdescri1,10)+cod_operado+DTOC(mdata)+TRANSFORM(mvalor,'999999.99'))
                        imprt('M','N',1)
                        SETPRC(00,00)
                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                ENDDO
                RESTSCREEN(00,00,24,79,mtela_aut)
        ENDIF
ENDDO
RETURN NIL
********************************** f i m **********************************
********************************
* CANCELAR/ALTERAR DESPESAS
********************************

FUNCTION saf31_3
*****************
LOCAL mprg:='SAF31_3'
LOCAL tela,lci,cci,lba,cba,li,ci,la,ca,point,opcao,m_codigo:={},m_posicao:={},mdata,;
      mvalor,mdescr1,mdescri2,mcoddesp,mtraco

IF ! ver_nivel(mprg,'CANCELAR OU ALTERAR DESPESAS','135',nivel_acess)
        RETURN NIL
ENDIF

lci := 06
cci := 02
lba := 22
cba := 77

li = 15
ci = 21
la = 22
ca = 79
mdata := mdata_sis
tela := SAVESCREEN(01,00,24,79)
*---------------------------------------------------------------
IF ! AbriArq('safcadde','desp');RETURN NIL;ENDIF
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
*---------------------------------------------------------------
WHILE .T.
        botao(lci,cci,lba,cba)
        @ lci+2,cci+12 SAY CHR(194)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+3,cci+12 TO lba-1,cci+12

        @ lci+2,cci+17 SAY CHR(194)
        @ lci+3,cci+17 TO lba-1,cci+17

        @ lci+2,cci+60 SAY CHR(194)
        @ lci+3,cci+60 TO lba-1,cci+60
        janela(lci,cba,' Cancelamento de Despesas ')
        DEVPOS(lci+1,cci+1);DEVOUT(' Data ')
        DEVPOS(lci+1,cci+13);DEVOUT('Tipo')
        DEVPOS(lci+1,cci+18);DEVOUT(' Descricao ')
        DEVPOS(lci+1,cci+61);DEVOUT('     Valor')

        DEVPOS(lci,cci+2);DEVOUT(' Digite a data da despesas: ')
        @ lci,cci+30 GET mdata
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF EMPTY(mdata)
                LOOP
        ENDIF

        *************
        SELE('caix');ORDSETFOCUS(1)
        GO TOP
        *************
        SEEK DTOS(mdata)

        IF ! FOUND()
                atencao('Nao existe nenhuma despesa !!!')
                LOOP
        ENDIF
        mensagem('Aguarde um momento OK !!!')
        ASIZE(m_codigo,0)
        ASIZE(m_posicao,0)
        WHILE ! EOF() .AND. mdata = data
                IF tipo = 'DE' .OR. tipo = 'DF'
                        AADD(m_codigo,' '+DTOC(data)+'  '+CHR(179)+' '+tipo+' '+CHR(179)+' '+descri1+' '+CHR(179)+' '+TRANSFORM(valor,'9,999,999.99'))
                        AADD(m_posicao,RECNO())
                        SKIP
                        LOOP
                ENDIF
                SKIP
        ENDDO

        IF LEN(m_codigo) = 0
                atencao('Nenhum despesa')
                LOOP
        ENDIF

        mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Confirma o cancelamento" )
        point := ACHOICE(lci+3,cci+1,lba-1,cba-1,m_codigo)

        DO CASE
                CASE LASTKEY()=27
                        LOOP
                CASE LASTKEY() = 13
                        GO m_posicao[point]
                        mensagem('Deseja [A]lterar ou [C]ancelar a DESPESA:')
                        opcao := SPACE(1)
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'A,C'
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        IF opcao = 'C'
                                mensagem('Confirma Cancelamento da despesa [s/N]:')
                                opcao = 'N'
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        LOOP
                                ELSE
                                        IF ! BLOQREG()
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                EXIT
                                        ENDIF
                                        DELE
                                        DBCOMMITALL()
                                        DBUNLOCKALL()
                                ENDIF
                        ELSE
                                IF ! BLOQREG()
                                        atencao('Nao foi possivel acessar o Arquivo !!!')
                                        EXIT
                                ENDIF
                                mtraco := REPLI('=',80)
                                setcor(3)
                                CAIXA(mold,li,ci,la,ca,,,.T.)
                                @ li+1,ci+1 TO li+1,ca-1
                                setcor(1)
                                SET INTEN ON
                                mensagem('Preencha os Campos - <ESC> p/Retornar ')
                                mdata := data
                                mvalor := valor*-1
                                mcoddesp := VAL(coddesp)
                                mdescri1 := descri1
                                mdescri2 := descri2
                                opcao = 'S'
                                janela(li,ca,' Alteracao de Despesas ')
                                DEVPOS(li+2,ci+2);DEVOUT('Data.......:')
                                DEVPOS(li+3,ci+2);DEVOUT('Valor R$...:')
                                DEVPOS(li+4,ci+2);DEVOUT('Cod.Despesa:')
                                DEVPOS(li+5,ci+2);DEVOUT('Descricao..:')

                                @ li+2,ci+15 GET mdata
                                @ li+3,ci+15 GET mvalor PICT '99,999,999.99' VALID IF(EMPTY(mvalor),.F.,.T.)
                                @ li+4,ci+15 GET mcoddesp PICT '999' VALID ver_desp(mcoddesp)
                                READ
                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela)
                                        CLOSE ALL
                                        RETURN NIL
                                ENDIF
                                @ li+5,ci+15 GET mdescri1 PICT '@!'
                                @ li+6,ci+15 GET mdescri2 PICT '@!'
                                READ
                                IF LASTKEY() = 27
                                        LOOP
                                ENDIF
                                mensagem('Confirma a Despesas [S/n]:')
                                opcao := 'S'
                                @ 23,COL() GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        LOOP
                                ENDIF
                                caix-> data := mdata
                                caix-> venci := mdata
                                caix-> documento := '9999999999'
                                caix-> valor := mvalor * -1
                                caix-> coddesp := STRZERO(mcoddesp,3)
                                caix-> descri1 := mdescri1
                                caix-> descri2 := mdescri2
                                COMMIT
                                UNLOCK
/*
                                IF set->despesa = 'F'
                                        FOR i = 1 TO set->via_desp
                                                SET DEVI TO PRINT
                                                SET PRINTER TO LPT1
                                                imprt('M','E',2)
                                                DEVPOS(PROW(),01);DEVOUT(memp+resa)
                                                imprt('M','N')
                                                DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm +' - '+ mfone_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+' -  C.G.C.: '+mcgc_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                                IF caix->tipo = 'DE'
                                                        DEVPOS(PROW()+2,25);DEVOUT('D E S P E S A S')
                                                        DEVPOS(PROW(),25);DEVOUT('D E S P E S A S')
                                                ELSE
                                                        DEVPOS(PROW()+2,25);DEVOUT('D E S P E S A S   ADM')
                                                        DEVPOS(PROW(),25);DEVOUT('D E S P E S A S   ADM')
                                                ENDIF
                                                DEVPOS(PROW()+2,00);DEVOUT('Data da Despesa:')
                                                DEVPOS(PROW(),17);DEVOUT(mdata)

                                                DEVPOS(PROW()+2,00);DEVOUT('Valor Despesa__:')
                                                DEVPOS(PROW(),17);DEVOUT(LTRIM(TRANSFORM(mvalor,'99,999,999.99')))

                                                IF ! EMPTY(mcoddesp)
                                                        DEVPOS(PROW()+2,00);DEVOUT('Descricao'+' ('+STRZERO(mcoddesp,3)+'):')
                                                ELSE
                                                        DEVPOS(PROW()+2,00);DEVOUT('Descricao______:')
                                                ENDIF
                                                DEVPOS(PROW(),17);DEVOUT(mdescri1)
                                                DEVPOS(PROW()+1,17);DEVOUT(mdescri2)

                                                DEVPOS(PROW()+4,00);DEVOUT(PADC('___________________________           ____________________________',80))
                                                DEVPOS(PROW()+1,00);DEVOUT(PADC('         C A I X A                              Solicitante       ',80))

                                                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                                                IF set->via_vales > 1 .AND. i < set->via_vales
                                                        DEVPOS(PROW()+3,00);DEVOUT(' ')
                                                ENDIF
                                        NEXT
                                        EJECT
                                        SETPRC(00,00)
                                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                                ELSEIF set->despesa = 'B'
*/
                                        mtraco := REPLI('=',54)
                                        FOR i = 1 TO 2
                                                SET DEVI TO PRINT
                                                SET PRINTER TO LPT1
                                                imprt('M','C')
                                                DEVPOS(PROW(),01);DEVOUT(memp+resa)
                                                DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm +' - '+ mfone_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+' -  C.G.C.: '+mcgc_firm)
                                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)

                                                IF caix->tipo = 'DE'
                                                        DEVPOS(PROW()+2,00);DEVOUT('D E S P E S A S')
                                                ELSE
                                                        DEVPOS(PROW()+2,00);DEVOUT('D E S P E S A S   ADM')
                                                ENDIF

                                                DEVPOS(PROW()+2,00);DEVOUT('Data da Despesa:')
                                                DEVPOS(PROW(),17);DEVOUT(mdata)

                                                DEVPOS(PROW()+2,00);DEVOUT('Valor Despesa..:')
                                                DEVPOS(PROW(),17);DEVOUT(LTRIM(TRANSFORM(mvalor,'99,999,999.99')))

                                                IF ! EMPTY(mcoddesp)
                                                        DEVPOS(PROW()+2,00);DEVOUT('Descricao'+' ('+STRZERO(mcoddesp,3)+'):')
                                                ELSE
                                                        DEVPOS(PROW()+2,00);DEVOUT('Descricao......:')
                                                ENDIF
                                                DEVPOS(PROW(),17);DEVOUT(mdescri1)
                                                DEVPOS(PROW()+1,17);DEVOUT(mdescri2)

                                                DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                                                DEVPOS(PROW()+1,00);DEVOUT(PADC('         C A I X A         ',60))
                                                DEVPOS(PROW()+3,00);DEVOUT(PADC('...........................',60))
                                                DEVPOS(PROW()+1,00);DEVOUT(PADC('         Solicitante       ',60))

                                                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                                        NEXT
                                        DEVPOS(PROW()+15,00);DEVOUT(' ')
                                        imprt('M','N')
                                        SETPRC(00,00)
                                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
*                               ENDIF
                        ENDIF
        ENDCASE
ENDDO
******************************** f i m *************************************
