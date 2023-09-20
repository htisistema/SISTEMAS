*******************************
* BAIXA DE PAGAMENTOS
*******************************

FUNCTION saf201(cx)
**************
LOCAL mprg:='SAF201'
LOCAL tela,tela1,opcao,lci,cci,lba,cba,li,ci,la,ca,pconta
LOCAL mparcela,mvlr_pago,mdatapag,mtipo_pag,;
      mdesc_bolsa,mcont_doc,mtotal_doc,mvalor,mbolsa,mano:=SPACE(4),;
      mdata_venc,mvlr_conta,m_dup:={},m_po:={},mmulta:=0,tela_aut,mtipo:=SPACE(2),;
      mdesconto:=0,mvlr_multa:=0,mvlr_juros:=0,mvlr_igpm:=0,mdias:=0
LOCAL m_cdmen:={},m_pos:={},point,f,i
PRIVATE mgstt,msituacao,mtipo_sit,mtipo_imp:='M'
PRIVATE mmatricula
IF cx = NIL
        IF ! ver_nivel(mprg,'BAIXA DE MENSALIDADES P/MOVIMENTO','125')
                RETURN NIL
        ENDIF
ELSE
        IF ! ver_nivel(mprg+'CX','BAIXA DE MENSALIDADES P/CAIXA','125')
                RETURN NIL
        ENDIF
ENDIF
tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 08
lba := 18
cba := 72

li := 03
ci := 29
la := 20
ca := 79
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safigpm','igpm');RETURN NIL;ENDIF
IF ! abriarq('safsetup','set');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('safcaixa','caix');RETURN NIL;ENDIF
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
        mtipo       := SPACE(2)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' Baixa de Mensalidades ')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+2,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+3,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+3,cci+30);DEVOUT('Bolsa:')
        DEVPOS(lci+4,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+4,cci+30);DEVOUT('Ano Competencia..:')
        DEVPOS(lci+5,cci+1);DEVOUT('Valor Parcela...R$:')
        DEVPOS(lci+5,cci+30);DEVOUT('Desconto Bolsa.R$:')
        DEVPOS(lci+6,cci+1);DEVOUT('Data Vencimento...:')
        DEVPOS(lci+7,cci+1);DEVOUT('Valor a Multa...R$:')
        DEVPOS(lci+8,cci+1);DEVOUT('Valor a Juros...R$:')
        DEVPOS(lci+8,cci+30);DEVOUT('Dias em Atraso...:')
        DEVPOS(lci+9,cci+1);DEVOUT('Valor a IGPM....R$:')
        DEVPOS(lci+10,cci+1);DEVOUT('Valor a Pagar...R$:')
        DEVPOS(lci+11,cci+1);DEVOUT('Data Pagamento....:')
        DEVPOS(lci+12,cci+1);DEVOUT('[C]arteira [B]anco:')
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
                                IF ! EMPTY(cdmen->data_pag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_cdmen,cdmen->ano+'     '+cdmen->parcela+'       '+cdmen->tipo+'  '+DTOC(cdmen->data_venc)+'   '+TRANSFORM(cdmen->vlr_parc-cdmen->desc_bolsa,'9,999.99')+' '+cdmen->bolsa+' '+cdmen->conta)
                                AADD(m_pos,RECNO())
                                mcont_doc ++
                                mtotal_doc := mtotal_doc + (cdmen->vlr_parc-cdmen->desc_bolsa)
                                SKIP
                        ENDDO
                        IF LEN(m_cdmen) = 0
                                atencao('Nao existe nenhuma Parcela a ser baixada')
                                EXIT
                        ENDIF
                        mensagem('<> Consulta  - <ESC> Retorna  -  <ENTER> p/Confirmar')
                        setcor(1)
                        botao(li,ci,la,ca,,' Demonstrativo de Parcelas ')
                        DEVPOS(li+1,ci+01);DEVOUT(' Ano')
                        DEVPOS(li+1,ci+08);DEVOUT('Parcela  ')
                        DEVPOS(li+1,ci+19);DEVOUT('TP')
                        DEVPOS(li+1,ci+23);DEVOUT('Vencimento')
                        DEVPOS(li+1,ci+37);DEVOUT('Valor')
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
                                        mdesconto := 0
                                        setcor(3)
                                        IF cdmen->tipo = 'ME'
                                                IF ! ver_fx(cdmen->bolsa)
                                                        LOOP
                                                ENDIF
                                                DEVPOS(lci+3,cci+37);DEVOUT(ver_faixa(cdmen->parcela)+' - (%): ')
                                                DEVPOS(lci+3,COL()+1);DEVOUTPICT(fx->desconto,'999.99')
                                                mdesconto := fx->desconto
                                        ENDIF
                                        ASIZE(m_dup,0)
                                        ASIZE(m_po,0)
                                        pconta      := ' '
                                        mvlr_conta  := 0
                                        mvlr_pago   := 0
                                        mdesc_bolsa := 0
                                        mvalor      := 0
                                        mdatapag    := DATE()
                                        mparcela    := cdmen->parcela
                                        mbolsa      := cdmen->bolsa
                                        mdata_venc  := cdmen->data_venc
                                        mtipo       := cdmen->tipo
                                        mano        := cdmen->ano
*                                       IF ! ver_tab(SUBSTR(mgstt,1,2)+SUBSTR(mgstt,4),cdmen->parcela)
*                                               LOOP
*                                       ENDIF
                                        IF mdesconto = 0
                                                mvalor    := cdmen->vlr_parc
                                                mvlr_pago := mvalor
                                        ELSE
                                                mvalor    := cdmen->vlr_parc - (cdmen->vlr_parc * (mdesconto/100))
                                                mvlr_pago := mvalor
                                        ENDIF
                                        mvlr_multa := 0
                                        mvlr_juros := 0
                                        mvlr_igpm  := 0
                                        mdias := (mdata_sis - cdmen->data_venc)
                                        ver_igpm(cdmen->parcela)
                                        IF mdias > 0
                                                mvlr_multa := mvalor*(set->vlr_multa/100)
                                                mvlr_juros := mdias * (mvalor * set->vlr_juros)
                                                IF mdias > 30
                                                        mvlr_igpm := mvlr_igpm + (mvalor*(igpm->vlr_igpm/100))
                                                ENDIF
                                        ENDIF
                                        mvlr_pago := mvlr_pago + mvlr_multa + mvlr_juros + mvlr_igpm
                                        setcor(3)
                                        DEVPOS(lci+4,cci+21);DEVOUT(cdmen->parcela)
                                        DEVPOS(lci+4,cci+49);DEVOUT(cdmen->ano)
                                        DEVPOS(lci+5,cci+21);DEVOUTPICT(cdmen->vlr_parc,'9,999.99')
                                        mdesc_bolsa := cdmen->vlr_parc - mvalor
                                        DEVPOS(lci+5,cci+49);DEVOUTPICT(mvalor - cdmen->vlr_parc,'9,999.99')
                                        DEVPOS(lci+6,cci+21);DEVOUT(cdmen->data_venc)
                                        DEVPOS(lci+7,cci+21);DEVOUTPICT(mvlr_multa,'9,999.99')
                                        DEVPOS(lci+8,cci+21);DEVOUTPICT(mvlr_juros,'9,999.99')
                                        DEVPOS(lci+8,cci+49);DEVOUTPICT(mdias,'9,999.99')
                                        DEVPOS(lci+9,cci+21);DEVOUTPICT(mvlr_igpm,'9,999.99')
                                        setcor(1)
                                        @ lci+10,cci+21  GET mvlr_pago PICT '9,999.99'
                                        @ lci+11,cci+21  GET mdatapag
                                        @ lci+12,cci+21  GET mtipo_pag PICT '@!' VALID mtipo_pag $ 'C,B,E'
                                        READ
                                        IF LASTKEY()=27
                                                LOOP
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
                                        IF mvlr_pago < cdmen->vlr_parc + mdesc_bolsa .AND. mdesc_bolsa = 0
                                                mensagem('Este Valor vai ser por CONTA [S/n]:')
                                                pconta := 'S'
                                                @ MAXROW()-1,COL()+1 GET pconta PICT '@!' VALID pconta $ 'S,N'
                                                READ
                                                IF LASTKEY() = 27
                                                        LOOP
                                                ENDIF
                                                IF pconta = 'N'
                                                        IF ! aut_sen('Senha de autorizacao p/dispensar o restante da parcela:')
                                                                LOOP
                                                        ENDIF
                                                ENDIF
                                        ENDIF
                                        IF BLOQREG()
                                                mmulta := 0
                                                cdmen->data_pro   := DATE()
                                                cdmen->data_pag   := mdatapag
                                                cdmen->valor      := mvlr_pago
                                                IF mvlr_pago > mvalor
                                                        mmulta := mvlr_pago - mvalor
                                                        cdmen->multa := mvlr_pago - mvalor
                                                ELSE
                                                        cdmen->multa := 0
                                                        mmulta := 0
                                                ENDIF
                                                cdmen->desc_bolsa := mdesc_bolsa
                                                cdmen->flag       := mtipo_pag
                                                cdmen->operador   := cod_operado
                                                IF pconta = 'S'
                                                        pconta       := '*'
                                                        cdmen->conta := '*'
                                                        mvlr_conta   := (cdmen->vlr_parc - mdesc_bolsa) - mvlr_pago
                                                ENDIF
                                                DBCOMMIT()
                                                DBUNLOCK()
                                                AADD(m_dup,mmatricula+' '+mtipo+' '+mmatricula+'/'+mparcela+' '+mano+' '+DTOC(mdata_venc)+' '+TRANSFORM(cdmen->vlr_parc,'9,999.99')+'  '+TRANSFORM(mmulta,'999.99')+'  '+TRANSFORM(mvlr_pago,'9,999.99')+' ')
                                                AADD(m_po,{RECNO(),'X',cdmen->vlr_parc,mmulta,mvlr_pago,'X'})
                                                mflag := 'S'
                                        ELSE
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                LOOP
                                        ENDIF
                                        IF pconta = '*'
                                                IF ADIREG()
                                                        cdmen-> ano        := mano
                                                        cdmen-> contrato   := aluno->contrato
                                                        cdmen-> tipo       := mtipo
                                                        cdmen-> gstt       := mgstt
                                                        cdmen-> matricula  := mmatricula
                                                        cdmen-> bolsa      := mbolsa
                                                        cdmen-> parcela    := mparcela
                                                        cdmen-> data_venc  := mdata_venc
                                                        cdmen-> vlr_parc   := mvlr_conta
                                                        cdmen-> desc_bolsa := mdesc_bolsa
                                                        cdmen-> operador   := cod_operado
                                                        cdmen-> conta      := '*'
                                                ENDIF
                                                DBCOMMIT()
                                                DBUNLOCK()
                                        ENDIF
                                        IF cx <> NIL
                                                ******************
                                                SELE('caix');ORDSETFOCUS(3)
                                                GO TOP
                                                *****************
                                                IF ADIREG()
                                                        caix-> ano  := mano
                                                        caix-> tipo := mtipo
                                                        caix-> num_dup := mmatricula+'/'+mparcela
                                                        caix-> nota    := mmatricula+'/'+mparcela
                                                        caix-> documento := mmatricula+'/'+mparcela
                                                        caix-> data := mdatapag
                                                        caix-> venci := mdatapag
                                                        caix-> valor := mvlr_pago
                                                        caix-> cod_cli := mmatricula
*                                                       caix-> nota := mnota
                                                        caix-> cod_vend := cod_operado
                                                        caix-> cod_opera := cod_operado
                                                        caix-> pg := '*'
                                                        caix-> op_pg := 'P'
                                                        caix-> valor_com := mmulta
                                                        DBCOMMIT()
                                                        DBUNLOCK()
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
                                                                rec_dupr(m_dup,m_po,1,mmulta,mvlr_pago,mdatapag)
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
                                                                DEVPOS(00,00);DEVOUT(maut+mtipo+ALLTRIM(mmatricula)+mparcela+mano+cod_operado+DTOC(mdatapag)+TRANSFORM(mvlr_pago,'999999.99'))
                                                                imprt('M','N',1)
                                                                SETPRC(00,00)
                                                                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                                                        ENDDO
                                                        RESTSCREEN(00,00,24,79,mtela_aut)
                                                ENDIF
                                        ENDIF
                                        EXIT
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhuma Parcela a ser baixada')
                        EXIT
                ENDIF
        ENDDO
ENDDO
RETURN NIL
********************************* F I M ********************************
************************************************
*FUNCAO P/IMPRIMIR RECIBO BAIXA DE DOC.RECEBER
************************************************
FUNCTION rec_dupr(m_d,m_p,mqtd,mtot_m,mtot,mdat,mtipo_rel)
********************

LOCAL mtraco := REPLI('=',54),i:=0,mtipo_imp:='M',mextenso,mlinha,mlin,lin
GO m_p[1,1]
SET DEVICE TO PRINT
SET PRINT TO LPT1
*SET PRINT TO RECDUPR.REL
*IF cfg->tipfrmped = 'B'
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mend_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mcid_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM('Fone: '+mfone_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        IF mtipo_rel = NIL
                DEVPOS(PROW()+1,00);DEVOUT(PADC('RECIBO PAGAMENTO',54))
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT(PADC('*** SEM VALOR FISCAL ***',54))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(PADC('DEMONSTRATIVO DE DEBITO',54))
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Data Emissao: '+DTOC(mdata_sis)+'     Data do Pagamento: '+DTOC(mdat))
        DEVPOS(PROW()+1,00);DEVOUT('Matricula..: '+aluno->matricula)
        DEVPOS(PROW()+1,00);DEVOUT('Aluno......:'+aluno->aluno)
        DEVPOS(PROW()+1,00);DEVOUT('Responsavel: '+aluno->responsa)
        DEVPOS(PROW()+1,00);DEVOUT('CPF........: '+aluno->r_cpf)
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Documento')
        DEVPOS(PROW(),14);DEVOUT('ANO')
        DEVPOS(PROW(),19);DEVOUT('Vencimen')
        DEVPOS(PROW(),32);DEVOUT('Valor')
        DEVPOS(PROW(),38);DEVOUT('Multa')
        DEVPOS(PROW(),45);DEVOUT('Vlr.Total')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        FOR i = 1 TO LEN(m_d)
                IF EMPTY(m_d[i])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(SUBSTR(m_d[i],7))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',54))
        DEVPOS(PROW()+1,00);DEVOUT('Qtd.Doc.: '+TRANSFORM(mqtd,'99999'))
        DEVPOS(PROW(),24);DEVOUT('Valor Total R$: '+TRANSFORM(mtot,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        mextenso:= '('+RTRIM(EXT(ROUND(mtot,2),10))+')'
        mlinha := MLCOUNT(mextenso,54)
        lin := 0
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,54,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),54))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Recebemos o valor acima descrito, pelo qual damos quitacao.')
        DEVPOS(PROW()+3,00);DEVOUT(PADC('----------------------------------------',54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('       Local e data do recebimento      ',54))
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp+resa))+4),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),54))
        imprt(mtipo_imp,'N',11)
        SETPRC(00,00)
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
/*
ELSEIF cfg->tipfrmped = 'F' .OR. cfg->tipfrmped = 'G'
        mtraco := REPLI('=',80)
        imprt(mtipo_imp,'E')
        DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(set->end)+'-'+RTRIM(set->bairro)+'-'+RTRIM(set->cidade)+'-'+RTRIM(set->uf)+' - Fone: '+set->fone)
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        IF mtipo_rel = NIL
                DEVPOS(PROW()+1,00);DEVOUT(PADC('RECIBO PAGAMENTO',80))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(PADC('DEMONSTRATIVO DE DEBITO',80))
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Data: '+DTOC(mdata_sis)+'        Data do Pagamento: '+DTOC(mdat))
        DEVPOS(PROW()+1,00);DEVOUT('Cliente: '+dupr->fornec+'-'+dupr->cliente)
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Documento')
        DEVPOS(PROW(),14);DEVOUT('Vencimen')
        DEVPOS(PROW(),28);DEVOUT('Valor')
        DEVPOS(PROW(),34);DEVOUT('Juros+Mult')
        DEVPOS(PROW(),46);DEVOUT('Vlr.Total')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        FOR i = 1 TO LEN(m_d)
*               IF m_p[i,2] <> 'X'
*                       LOOP
*               ENDIF
                IF EMPTY(m_d[i])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(SUBSTR(m_d[i],7,55))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
        DEVPOS(PROW()+1,00);DEVOUT('Qtd.Doc.: '+TRANSFORM(mqtd,'99999'))
        DEVPOS(PROW(),45);DEVOUT(TRANSFORM(mtot,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        mextenso:= '('+RTRIM(EXT(ROUND(mtot,2),10))+')'
        mlinha := MLCOUNT(mextenso,60)
        lin := 0
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,60,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),60))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Recebemos o valor acima descrito, pelo qual damos quitacao.')
        DEVPOS(PROW()+3,00);DEVOUT(PADC('----------------------------------------',80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('       Local e data do recebimento      ',80))
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp+resa))+4),80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),80))
        EJECT
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDIF
*/
*lertexto('RECDUPR.REL')
RETURN NIL


