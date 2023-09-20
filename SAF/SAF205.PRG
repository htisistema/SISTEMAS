*******************************
* EXCLUSAO DE DOCUMENTOS MENSALIDADES
*******************************

FUNCTION saf205
**************
LOCAL mprg:='SAF205'
LOCAL tela,tela1,opcao,lci,cci,lba,cba,li,ci,la,ca,pconta
LOCAL mmatricula,mparcela,mvlr_pago,mdatapag,mtipo_pag,;
      mdesc_bolsa,mcont_doc,mtotal_doc,mvalor,mbolsa,;
      mdata_venc,mvlr_conta,mdesconto:=0
LOCAL m_cdmen:={},m_pos:={},point,f,i
PRIVATE mgstt,msituacao,mtipo_sit

IF ! ver_nivel(mprg,'EXCLUSAO MENSALIDADES','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 08
lba := 15
cba := 72

li := 03
ci := 42
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
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+2,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+3,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+3,cci+30);DEVOUT('Bolsa:')
        DEVPOS(lci+4,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+5,cci+1);DEVOUT('Valor Parcela...R$:')
        DEVPOS(lci+5,cci+30);DEVOUT('Desconto Bolsa.R$:')
        DEVPOS(lci+6,cci+1);DEVOUT('Data Vencimento...:')
        setcor(1)
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        @ lci+1,cci+21 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.F.,.T.)
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
        DEVPOS(lci+2,cci+21);DEVOUT(aluno->aluno)
        DEVPOS(lci+3,cci+21);DEVOUT(aluno->gstt)
        mgstt := aluno->gstt

        SETCOR(1)
        tela1 := SAVESCREEN(02,40,22,79)
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
                                IF ! EMPTY(cdmen->data_pag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_cdmen,'  '+cdmen->parcela+'        '+DTOC(cdmen->data_venc)+'   '+TRANSFORM(cdmen->vlr_parc,'9,999.99')+' '+cdmen->conta)
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
                        botao(li,ci,la,ca)
                        DEVPOS(li+1,ci+01);DEVOUT('Parcela  ')
                        DEVPOS(li+1,ci+12);DEVOUT('Vencimento')
                        DEVPOS(li+1,ci+26);DEVOUT('Valor')
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
                                        RESTSCREEN(02,40,22,79,tela1)
                                        EXIT
                                CASE LASTKEY() = 13
                                        RESTSCREEN(02,40,22,79,tela1)
                                        GO m_pos[point]
                                        pconta    := ' '
                                        setcor(3)
*                                       IF ! ver_fx(ver_faixa(cdmen->parcela))
                                        pconta      := ' '
                                        mvlr_conta  := 0
                                        mvlr_pago   := 0
                                        mdesconto   := 0
                                        mdesc_bolsa := 0
                                        mvalor      := 0
                                        mdatapag    := DATE()
                                        mparcela    := cdmen->parcela
                                        mbolsa      := cdmen->bolsa
                                        mdata_venc  := cdmen->data_venc
                                        IF cdmen->tipo = 'ME'
                                                IF ! ver_fx(cdmen->bolsa)
                                                        LOOP
                                                ENDIF
                                                DEVPOS(lci+3,cci+37);DEVOUT(ver_faixa(cdmen->parcela)+' - (%): ')
                                                DEVPOS(lci+3,COL()+1);DEVOUTPICT(fx->desconto,'999.99')
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
                                        DEVPOS(lci+4,cci+21);DEVOUT(cdmen->parcela)
                                        DEVPOS(lci+5,cci+21);DEVOUTPICT(cdmen->vlr_parc,'9,999.99')
                                        mdesc_bolsa := cdmen->vlr_parc - mvalor
                                        DEVPOS(lci+5,cci+49);DEVOUTPICT(mvalor - cdmen->vlr_parc,'9,999.99')
                                        DEVPOS(lci+6,cci+21);DEVOUT(mdata_venc)
                                        READ
                                        IF LASTKEY()=27
                                                LOOP
                                        ENDIF
                                        Mensagem('Confirma a Exclusao dos dados ? [S/n]:')
                                        opcao := 'N'
                                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                        READ
                                        IF LASTKEY() = 27
                                                LOOP
                                        ENDIF
                                        IF opcao = 'N'
                                                LOOP
                                        ENDIF
                                        IF BLOQREG()
                                                DELE
                                                DBCOMMIT()
                                                DBUNLOCK()
                                        ELSE
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                LOOP
                                        ENDIF
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhuma Parcela a ser baixada')
                        EXIT
                ENDIF
        ENDDO
ENDDO
RETURN NIL
