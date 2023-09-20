*******************************
* 2a.VIA DO RECIBO PAG. OU AUTENTICAR
*******************************

FUNCTION saf32
**************
LOCAL mprg:='SAF32'
LOCAL tela,tela1,opcao,lci,cci,lba,cba,li,ci,la,ca,pconta
LOCAL mmatricula,mparcela,mvlr_pago,mdatapag,mtipo_pag,;
      mdesc_bolsa,mcont_doc,mtotal_doc,mvalor,mbolsa,;
      mdata_venc,mvlr_conta,mano,m_dup:={},m_po:={}
LOCAL m_cdmen:={},m_pos:={},point,f,i
PRIVATE mgstt,msituacao,mtipo_sit

IF ! ver_nivel(mprg,'2a.VIA DO RECIBO PAG. OU AUTENTICAR','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 08
lba := 15
cba := 72

li := 03
ci := 26
la := 20
ca := 79
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mano        := SPACE(4)
        mmatricula  := SPACE(6)
        mparcela    := SPACE(2)
        mvlr_pago   := 0
        mdatapag    := DATE()
        mtipo_pag   := 'C'
        mdesc_bolsa := 0
        mcont_doc   := 0
        mtotal_doc  := 0
        mvalor      := 0
        mbolsa      := SPACE(1)
        mdata_venc  := CTOD('  /  /  ')
        mvlr_conta  := 0
        pconta      := SPACE(1)

        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' 2a.VIA RECIBO OU AUTENTICAR ')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Ano de Competencia:')
        DEVPOS(lci+2,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+4,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+4,cci+30);DEVOUT('Bolsa:')
        DEVPOS(lci+5,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+6,cci+1);DEVOUT('Valor Pago......R$:')
        DEVPOS(lci+7,cci+1);DEVOUT('Data Pagamento....:')
        setcor(1)
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        @ lci+1,cci+21 GET mano PICT '9999'
        @ lci+2,cci+21 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! aluno->(DBSEEK(mmatricula))
                atencao('Matricula nao encontrada no arquivo')
                LOOP
        ENDIF
        setcor(3)
        DEVPOS(lci+3,cci+21);DEVOUT(aluno->aluno)
        DEVPOS(lci+4,cci+21);DEVOUT(aluno->gstt)
        mgstt := aluno->gstt

        SETCOR(1)
        tela1 := SAVESCREEN(00,00,24,79)
        WHILE .T.
                mtotal_doc := 0
                mcont_doc  := 0
                ASIZE(m_cdmen,0)
                ASIZE(m_pos,0)
                ***************
                SELE('cdmen');ORDSETFOCUS(7)
                GO TOP
                ***************
                IF cdmen->(DBSEEK(aluno->matricula))
                        WHILE cdmen->matricula = aluno->matricula .AND. ! EOF()
                                IF ! EMPTY(mano)
                                        IF mano <> cdmen->ano
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF EMPTY(cdmen->data_pag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_cdmen,' '+cdmen->ano+' '+cdmen->tipo+'  '+cdmen->parcela+'        '+DTOC(cdmen->data_pag)+'   '+TRANSFORM(cdmen->vlr_parc,'9,999.99')+' '+cdmen->conta)
                                AADD(m_pos,RECNO())
                                mcont_doc ++
                                mtotal_doc := mtotal_doc + cdmen->vlr_parc
                                SKIP
                        ENDDO
                        IF LEN(m_cdmen) = 0
                                atencao('Nao existe nenhuma Parcela a ser baixada')
                                EXIT
                        ENDIF
                        mensagem('<> Consulta  - <ESC> Retorna  -  <ENTER> p/Confirmar')
                        setcor(1)
                        botao(li,ci,la,ca,,' DOCUMENTOS P/2A.VIA OU AUTENTICAR ')
                        DEVPOS(li+1,ci+01);DEVOUT('ANO')
                        DEVPOS(li+1,ci+06);DEVOUT('TP')
                        DEVPOS(li+1,ci+09);DEVOUT('Parcela  ')
                        DEVPOS(li+1,ci+20);DEVOUT('Data Pago ')
                        DEVPOS(li+1,ci+34);DEVOUT('Valor')
                        @ li+2,ci+1 TO li+2,ca-1
                        @ la-2,ci+1 TO la-2,ca-1
                        DEVPOS(la-1,ci+1);DEVOUT('Quantidade:')
                        DEVPOS(la-1,ci+20);DEVOUT('Total:')
                        setcor(1)
                        DEVPOS(la-1,ci+13);DEVOUT(STRZERO(mcont_doc,4))
                        DEVPOS(la-1,ci+27);DEVOUT(TRANSFORM(mtotal_doc,'99,999.99'))
                        point := ACHOICE(li+3,ci+1,la-3,ca-1,m_cdmen,,,point)

                        DO CASE
                                CASE LASTKEY()=27
                                        RESTSCREEN(00,00,24,79,tela1)
                                        EXIT
                                CASE LASTKEY() = 13
                                        RESTSCREEN(00,00,24,79,tela1)
                                        ASIZE(m_dup,0)
                                        ASIZE(m_po,0)
                                        GO m_pos[point]
                                        IF cdmen->valor > cdmen->vlr_parc
                                                mmulta := cdmen->valor - cdmen->vlr_parc
                                        ELSE
                                                mmulta := 0
                                        ENDIF
                                        AADD(m_dup,cdmen->matricula+' '+cdmen->tipo+' '+cdmen->matricula+'/'+cdmen->parcela+' '+cdmen->ano+' '+DTOC(cdmen->data_venc)+' '+TRANSFORM(cdmen->vlr_parc,'9,999.99')+'  '+TRANSFORM(mmulta,'999.99')+'  '+TRANSFORM(cdmen->valor,'9,999.99')+' ')
                                        AADD(m_po,{RECNO(),'X',cdmen->vlr_parc,mmulta,cdmen->valor,'X'})
                                        setcor(3)
                                        DEVPOS(lci+5,cci+21);DEVOUT(cdmen->parcela)
                                        DEVPOS(lci+6,cci+21);DEVOUTPICT(cdmen->valor,'9,999.99')
                                        DEVPOS(lci+7,cci+21);DEVOUT(cdmen->data_pag)
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
                                                        rec_dupr(m_dup,m_po,1,mmulta,cdmen->valor,cdmen->data_pag)
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
                                                        DEVPOS(00,00);DEVOUT(maut+cdmen->tipo+ALLTRIM(cdmen->matricula)+cdmen->parcela+cdmen->ano+cod_operado+DTOC(cdmen->data_pag)+ALLTRIM(TRANSFORM(cdmen->valor,'999999.99')))
                                                        imprt('M','N',1)
                                                        SETPRC(00,00)
                                                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                                                ENDDO
                                                RESTSCREEN(00,00,24,79,mtela_aut)
                                        ENDIF
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhuma Parcela a ser AUTENTICAR')
                        EXIT
                ENDIF
        ENDDO
ENDDO
RETURN NIL
