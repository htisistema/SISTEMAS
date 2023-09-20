*******************************
* ALTERACAO DE DOCUMENTOS MENSALIDADES
*******************************

FUNCTION saf204
**************
LOCAL mprg:='SAF204'
LOCAL tela,tela1,opcao,lci,cci,lba,cba,li,ci,la,ca,pconta
LOCAL mmatricula,mparcela,mvlr_pago,mdatapag,mtipo_pag,;
      mdesc_bolsa,mcont_doc,mtotal_doc,mvalor,mbolsa,;
      mdata_venc,mvlr_conta,mano,mvlr_parc:=0,mdesconto := 0
LOCAL m_cdmen:={},m_pos:={},point,f,i
PRIVATE mgstt,msituacao,mtipo_sit

IF ! ver_nivel(mprg,'BAIXA DE MENSALIDADES','125')
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
        botao(lci,cci,lba,cba,,' ALTERACAO DE DOCUMENTOS ')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Ano de Competencia:')
        DEVPOS(lci+2,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+4,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+4,cci+30);DEVOUT('Bolsa:')
        DEVPOS(lci+5,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+6,cci+1);DEVOUT('Valor Parcela...R$:')
        DEVPOS(lci+6,cci+30);DEVOUT('Desconto Bolsa.R$:')
        DEVPOS(lci+7,cci+1);DEVOUT('Data Vencimento...:')
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
                                IF mano <> cdmen->ano
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(cdmen->data_pag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_cdmen,' '+cdmen->ano+' '+cdmen->tipo+'  '+cdmen->parcela+'        '+DTOC(cdmen->data_venc)+'   '+TRANSFORM(cdmen->vlr_parc,'9,999.99')+' '+cdmen->conta)
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
                        botao(li,ci,la,ca,,' DOCUMENTOS P/ALTERACAO ')
                        DEVPOS(li+1,ci+01);DEVOUT('ANO')
                        DEVPOS(li+1,ci+06);DEVOUT('TP')
                        DEVPOS(li+1,ci+09);DEVOUT('Parcela  ')
                        DEVPOS(li+1,ci+20);DEVOUT('Vencimento')
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
                                        GO m_pos[point]
                                        pconta    := ' '
                                        setcor(3)
                                        pconta      := ' '
                                        mvlr_conta  := 0
                                        mvlr_pago   := 0
                                        mdesc_bolsa := 0
                                        mvalor      := 0
                                        mdatapag    := DATE()
                                        mparcela    := cdmen->parcela
                                        mbolsa      := cdmen->bolsa
                                        mdata_venc  := cdmen->data_venc
                                        mvlr_parc   := cdmen->vlr_parc
                                        mdesconto   := 0
                                        IF cdmen->tipo = 'ME'
                                                IF ! ver_fx(cdmen->bolsa)
                                                        LOOP
                                                ENDIF
                                                DEVPOS(lci+4,cci+37);DEVOUT(ver_faixa(cdmen->parcela)+' - (%): ')
                                                DEVPOS(lci+4,COL()+1);DEVOUTPICT(fx->desconto,'999.99')
                                                mdesconto := fx->desconto
                                        ENDIF
                                        IF mdesconto = 0
                                                mvalor    := cdmen->vlr_parc
                                                mvlr_pago := mvalor
                                        ELSE
                                                mvalor    := cdmen->vlr_parc - (cdmen->vlr_parc * (mdesconto/100))
                                                mvlr_pago := mvalor
                                        ENDIF
                                        setcor(3)
                                        DEVPOS(lci+5,cci+21);DEVOUT(cdmen->parcela)
                                        @ lci+6,cci+21 GET mvlr_parc PICT '9,999.99'
                                        @ lci+7,cci+21 GET mdata_venc
                                        READ
                                        IF LASTKEY()=27
                                                LOOP
                                        ENDIF
                                        IF cdmen->tipo = 'ME'
                                                mdesc_bolsa := mvlr_parc - mvalor
                                                DEVPOS(lci+6,cci+49);DEVOUTPICT(mvalor - mvlr_parc,'9,999.99')
                                        ENDIF
                                        Mensagem('Confirma os Dados ? [S/n]:')
                                        opcao := 'S'
                                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                        READ
                                        IF LASTKEY() = 27
                                                LOOP
                                        ENDIF
                                        IF opcao = 'N'
                                                LOOP
                                        ENDIF
                                        IF BLOQREG()
                                                cdmen->data_venc  := mdata_venc
                                                cdmen->vlr_parc   := mvlr_parc
                                                DBCOMMIT()
                                                DBUNLOCK()
                                        ELSE
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                LOOP
                                        ENDIF
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhuma Parcela a ser ALTERADA')
                        EXIT
                ENDIF
        ENDDO
ENDDO
RETURN NIL
