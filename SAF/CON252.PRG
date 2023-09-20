***************************************************
*** CON252.PRG: Baixa GERAL de Duplicatas (Receber)
***
***************************************************

FUNCTION con252(cx,mtp)
***************
LOCAL MPRG:='CON252'
LOCAL tela,lin,mvlpago,mdata1,mdata2,mdatapg,msele,morde,opcao,mbaixa,t_for,;
      mdias_atras:=0,mquantd_doc:=0,mtot_multa:=0,mtotal:=0,mpoint,mvlr_pg:=0,mresta:=0
LOCAL lci,cci,lba,cba,li,ci,la,ca,i:=0,mjuros:=0,mmulta:=0

LOCA mtipo,mnumero,mduplicata,mvalor_dup,mcod_cli,mcliente,mtip_cli,;
     memissao,mcod_vend,mnum_ped,mbanco,mcomissao,mobs,mvenc,mcodven:=0

PRIVATE m_dupr:={},m_pos:={},m_pag:={},m_cax:={}

IF mtp = NIL
        IF ! ver_nivel(mprg,'CONTA A RECEBER (BAIXA GERAL DOCUMENTOS)','15',nivel_acess)
                RETURN NIL
        ENDIF
ELSE
        IF ! ver_nivel(mprg,'CONTA A RECEBER (DEMONSTRATIVO DE DEBITO)','15',nivel_acess)
                RETURN NIL
        ENDIF
ENDIF

tela := SAVESCREEN(01,00,24,79)

lci := 03
cci := 05
lba := 10
cba := 78

li := 10
ci := 02
la := 22
ca := 78

lin=1
mfornec := 0
mforn_aux := 0
mduplicata := SPACE(10)
*------------------------------------------------------------------------------------
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
IF ! AbriArq('saccarta','car');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
IF ! AbriArq('saccaixa','caix');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
*****************
SELE('dupr');ORDSETFOCUS(4)
*****************
msele := SELE()
morde := INDEXORD()
WHILE .T.
        ASIZE(m_dupr,0)
        ASIZE(m_pos,0)
        ASIZE(m_pag,0)
        ASIZE(m_cax,0)
        mtipo := 0
        mbaixa := 'N'
        mdata1 := CTOD('  /  /  ')
        mdata2 := CTOD('  /  /  ')
        mdatapg := mdata_sis
        t_for = 0
        mvlr_pg := 0
        mresta := 0

        IF mtp = NIL
                botao(lci,cci,lba,cba,,' Baixa de Documentos GERAL (C.Receber)')
                Mensagem('Digite a data da BAIXA GERAL DOS CHEQUES. [ESC] Abandona.')
                DEVPOS(lci+2,cci+1);DEVOUT('Digite a Data Pagamento :')
                DEVPOS(lci+2,cci+40);DEVOUT('Valor Pagamento R$:')
        ELSE
                botao(lci,cci,lba,cba,,' Demonstrativo de DEBITO (C.Receber)')
                Mensagem('Digite a data do Demonstrativo de DEBITO. [ESC] Abandona.')
                DEVPOS(lci+2,cci+1);DEVOUT('Emissao do Demonstrativo:')
        ENDIF
        @ lci+5,cci+1 TO lci+5,cba-1
        DEVPOS(lci+1,cci+1);DEVOUT('Digite o Periodo........:          a')
        DEVPOS(lci+3,cci+1);DEVOUT('Digite o Cod.Cliente....:')
        DEVPOS(lci+4,cci+1);DEVOUT('Digite o Cod.Vendedor...:')
        mtipo := SPACE(2)
        @ lci+1,cci+27 GET mdata1       //VALID IF(EMPTY(mdata1),.F.,.T.)
        @ lci+1,COL()+3 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        @ lci+2,cci+27 GET mdatapg VALID IF(mdatapg < mdata1,.F.,.T.)
        @ lci+2,cci+60 GET mvlr_pg PICT '9,999,999.99' WHEN mtp = NIL
        @ lci+3,cci+27 GET t_for pict "99999" VALID lim_get() .AND. ver_cli(t_for,lci+3,cci+33) WHEN men_get(lci+4,cci+27,'Informe o cliente que deseja ou nao informe p/imprimir todos')
        @ lci+4,cci+27 GET mcodven pict "999" VALID lim_get() .AND. ven(mcodven,ci+4,cci+33) WHEN EMPTY(t_for) .AND. men_get(lci+5,cci+27,'Informe o Cod.Vendedor que deseja ou nao informe p/imprimir todos')
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF EMPTY(mdata1)
                mdata1 := CTOD('01/01/90')
                mdata2 := mdata_sis+365
                @ lci+1,cci+27 SAY mdata1
                @ lci+1,COL()+3 SAY mdata2
        ENDIF
        **************
        SELE('dupr');ORDSETFOCUS(4)
        GO TOP
        **************
        IF ! EMPTY(t_for)
                dupr->(DBSEEK(DTOS(mdata1)+STRZERO(t_for,5),.T.))
        ELSE
                dupr->(DBSEEK(DTOS(mdata1),.T.))
        ENDIF
        IF EOF()
                atencao('Nao existe movimento neste periodo !!!')
                LOOP
        ENDIF
        @ lci+6,cci+12 PROMPT '  GERAL  '
        @ lci+6,cci+23 PROMPT ' CHEQUES '
        @ lci+6,cci+34 PROMPT ' CARTOES '
        SET INTEN ON
        MENU TO mtipo
        IF mtipo = 3
                mcod_cartao := 0
                botao(12,21,14,60)
                DEVPOS(13,22);DEVOUT('Cod.Cartao:')
                @ 13,COL()+1 GET mcod_cartao PICT '999' VALID ver_cartao(mcod_cartao,13,COL()+1)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mjuros := 0
        mmulta := 0
        mquantd_doc := 0
        mtot_multa := 0
        mtotal := 0
        WHILE dupr->venc <= mdata2 .AND. ! EOF()
                IF ! EMPTY(t_for) .AND. STRZERO(t_for,5) <> dupr->fornec
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mcodven) .AND. STRZERO(mcodven,3) <> dupr->vendedor
                        SKIP
                        LOOP
                ENDIF
                IF mtipo = 2
                        IF tipo <> 'CH' .OR. ! EMPTY(dupr->datpag)
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF mtipo = 3
                        IF tipo <> 'CT' .OR. ! EMPTY(dupr->datpag)
                                SKIP
                                LOOP
                        ENDIF
                        IF mcod_cartao > 0 .AND. STRZERO(mcod_cartao,3) <> dupr->numero
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF mtipo = 1
                        IF ! EMPTY(dupr->datpag)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                mbaixa := 'S'
                mdias_atras := mdatapg-(dupr->venc+set->dia_venc)
                mjuros := 0
                mmulta := 0
                IF mdias_atras > 0
                        mdias_atras := mdatapg-dupr->venc
                        mmulta := dupr->valor * ((set->multa_dup)/100)
                        mjuros := (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                ENDIF
                mvlpago=dupr->valor+mjuros+mmulta
                AADD(m_dupr,dupr->fornec+' '+dupr->tipo+' '+dupr->duplicata+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'999,999.99')+' '+TRANSFORM(mjuros+mmulta,'999,999.99')+' '+TRANSFORM(mvlpago,'999,999.99')+dupr->conta)
                AADD(m_pos,{RECNO(),' ',dupr->valor,mjuros+mmulta,mvlpago})
                SKIP
        ENDDO
        IF LEN(m_dupr) = 0
                atencao('Nao existe documento neste periodo')
                LOOP
        ENDIF
        mensagem('<> Consulta  - <ESC> Retorna  -  <ENTER> p/Confirmar')
        botao(li,ci,la,ca)
        setcor(3)
        DEVPOS(li+1,ci+01);DEVOUT('Client')
        DEVPOS(li+1,ci+08);DEVOUT('Documento')
        DEVPOS(li+1,ci+21);DEVOUT('Vencimen')
        DEVPOS(li+1,ci+35);DEVOUT('Valor')
        DEVPOS(li+1,ci+41);DEVOUT('Juros+Mult')
        DEVPOS(li+1,ci+53);DEVOUT('Vlr.Total')
        @ li+2,ci+1 TO li+2,ca-1
        @ la-2,ci+1 TO la-2,ca-1
        setcor(1)
        DEVPOS(la-1,ci+1);DEVOUT('Qtd.Doc.:       Multa/Juros R$:             Total R$:')
        IF EMPTY(mvlr_pg)
                opcao := mensagem1('Deseja marcar todas os Documentos: ','N','S,N')
        ENDIF
        IF opcao = 'S' .OR. ! EMPTY(mvlr_pg)
                i := 0
                FOR i = 1 TO LEN(m_dupr)
                        m_dupr[i] := SUBSTR(m_dupr[i],1,62)+' X'
                        m_pos[i,2] := 'X'
                        mquantd_doc ++
                        mtot_multa := mtot_multa + m_pos[i,4]
                        mtotal := mtotal + m_pos[i,5]
                NEXT
        ENDIF
        mpoint := 0
        WHILE .T.
                setcor(3)
                DEVPOS(la-1,ci+11);DEVOUT(TRANSFORM(mquantd_doc,'99999'))
                DEVPOS(la-1,ci+33);DEVOUT(TRANSFORM(mtot_multa,'999,999.99'))
                DEVPOS(la-1,ci+58);DEVOUT(TRANSFORM(mtotal,'999,999.99'))
                setcor(1)
                mpoint:=ACHOICE(li+3,ci+1,la-3,ca-1,m_dupr,,,mpoint+1)
                DO CASE
                        CASE LASTKEY() = 13
                                IF m_pos[mpoint,2] = 'X'
                                        m_dupr[mpoint] := SUBSTR(m_dupr[mpoint],1,62)+'  '
                                        m_pos[mpoint,2] := ' '
                                        mquantd_doc --
                                        mtot_multa := mtot_multa - m_pos[mpoint,4]
                                        mtotal := mtotal - m_pos[mpoint,5]
                                ELSE
                                        m_dupr[mpoint] := SUBSTR(m_dupr[mpoint],1,62)+' X'
                                        m_pos[mpoint,2] := 'X'
                                        mquantd_doc ++
                                        mtot_multa := mtot_multa + m_pos[mpoint,4]
                                        mtotal := mtotal + m_pos[mpoint,5]
                                ENDIF
                        CASE LASTKEY() = 27
                                EXIT
                ENDCASE
        ENDDO
        i := 0
        FOR i = 1 TO LEN(m_dupr)
                IF m_pos[i,2] = 'X'
                        EXIT
                ENDIF
        NEXT
        IF i > LEN(m_dupr) .OR. m_pos[i,2] <> 'X'
                atencao('Nao tem nenhum Documento marcado')
                LOOP
        ENDIF
        IF mtp = NIL
                Mensagem('Confirma as BAIXAS ? [S/n]:')
        ELSE
                Mensagem('Confirma os DADOS ? [S/n]:')
        ENDIF
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY()=27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF EMPTY(mvlr_pg)
                mvlr_pg := mtotal
        ENDIF
        mquantd_doc:=0
        mtot_multa := 0
        mtotal := 0
        i := 0
        FOR i = 1 TO LEN(m_dupr)
                mbaixa := 'S'
                IF m_pos[i,2] <> 'X'
                        LOOP
                ENDIF
                GO m_pos[i,1]
                IF m_pos[i,5] > mvlr_pg .AND. ! EMPTY(mvlr_pg)
                        AADD(m_pag,dupr->fornec+' '+dupr->tipo+' '+dupr->duplicata+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'999,999.99')+' '+TRANSFORM(0,'999,999.99')+' '+TRANSFORM(mvlr_pg,'999,999.99')+' *')
                        AADD(m_cax,{dupr->fornec,dupr->tipo,dupr->duplicata,dupr->venc,dupr->valor,mvlr_pg,dupr->comissao,dupr->vendedor})
                        IF mtp = NIL
                                IF BLOQREG()
                                        dupr-> datpag := mdatapg
                                        dupr-> vlpago := mvlr_pg
                                        dupr-> pago := "B"
                                        dupr-> operador := cod_operado
                                        dupr-> conta := '*'
                                        IF cx <> NIL
                                                dupr->mov_cx := 'C'
                                        ELSE
                                                dupr->mov_cx := 'M'
                                        ENDIF
                                        DBUNLOCK()
                                        DBCOMMIT()
                                ENDIF
                        ENDIF
                        mquantd_doc ++
                        mtot_multa := mtot_multa + 0
                        mtotal := mtotal + mvlr_pg
                        mresta := m_pos[i,5] - mvlr_pg
                        mvlr_pg := mvlr_pg - m_pos[i,5]

                        mtipo       := dupr->tipo
                        mnumero     := dupr->numero
                        mduplicata  := dupr-> duplicata
                        mnumero     := dupr-> numero
                        mvalor_dup  := dupr-> valor_dup
                        mcod_cli    := dupr-> fornec
                        mcliente    := dupr-> cliente
                        mtip_cli    := dupr-> tip_cli
                        memissao    := dupr-> emissao
                        mcod_vend   := dupr-> vendedor
                        mnum_ped    := dupr-> num_ped
                        pconta      := dupr-> conta
                        mbanco      := dupr-> banco
                        mcomissao   := dupr-> comissao
                        cod_operado := dupr-> operador
                        mobs        := dupr-> obs
                        mvenc       := dupr-> venc
                        IF ADIREG() .AND. mtp = NIL
                                dupr-> tipo := mtipo
                                IF mtipo = 'CH'
                                        dupr-> numero := mnumero
                                ENDIF
                                dupr-> duplicata := mduplicata
                                dupr-> numero  := mnumero
                                dupr-> valor_dup := mvalor_dup
                                dupr-> fornec := mcod_cli
                                dupr-> cliente := mcliente
                                dupr-> tip_cli := mtip_cli
                                dupr-> emissao := memissao
                                IF ! EMPTY(mmulta) .OR. ! EMPTY(mjuros)
                                        dupr-> venc := mdatapg
                                ELSE
                                        dupr-> venc := mvenc
                                ENDIF
                                dupr-> valor := mresta
                                dupr-> pago := " "
                                dupr-> vendedor := mcod_vend
                                dupr-> num_ped := mnum_ped
                                dupr-> conta := '*'
                                dupr-> banco := mbanco
                                dupr-> comissao := mcomissao
                                dupr-> operador := cod_operado
                                dupr->obs := mobs
                        ENDIF
                        EXIT
                ENDIF
                IF BLOQREG()
                        AADD(m_pag,dupr->fornec+' '+dupr->tipo+' '+dupr->duplicata+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'999,999.99')+' '+TRANSFORM(m_pos[i,4],'999,999.99')+' '+TRANSFORM(m_pos[i,5],'999,999.99'))
                        AADD(m_cax,{dupr->fornec,dupr->tipo,dupr->duplicata,dupr->venc,dupr->valor,m_pos[i,5],dupr->comissao,dupr->vendedor})
                        IF mtp = NIL
                                dupr-> datpag := mdatapg
                                dupr-> vlpago := m_pos[i,5]
                                IF vlpago<valor
                                        dupr-> desc := m_pos[i,3] - m_pos[i,5]
                                ELSE
                                        dupr-> juros := m_pos[i,4]
                                ENDIF
                                dupr-> pago := "B"
                                dupr-> operador := cod_operado
                                IF cx <> NIL
                                        dupr->mov_cx := 'C'
                                ELSE
                                        dupr->mov_cx := 'M'
                                ENDIF
                                DBUNLOCK()
                                DBCOMMIT()
                        ENDIF
                        mquantd_doc ++
                        mtot_multa := mtot_multa + m_pos[i,4]
                        mtotal := mtotal + m_pos[i,5]
                        mvlr_pg := mvlr_pg - m_pos[i,5]
                ELSE
                        atencao('Nao foi possivel acessar o Arquivo !!!')
                        LOOP
                ENDIF
                *****************
                SELE(msele);ORDSETFOCUS(morde)
                *****************
                DBCOMMITALL()
                SKIP
        NEXT
        IF cx <> NIL
                i := 0
                FOR i = 1 TO LEN(m_cax)
                        IF m_cax[i,2] = 'DU'
                                ******************
                                SELE('caix');ORDSETFOCUS(3)
                                GO TOP
                                *****************

                                SEEK m_cax[i,2] + m_cax[i,3] + m_cax[i,1]

                                IF FOUND()
                                        mnota := nota
                                        mtipo_comp := tipo_comp
                                        mcod_vend := cod_vend
                                        IF BLOQREG()
                                                caix->op_pg := 'P'
                                                DBCOMMIT()
                                                DBUNLOCK()
                                        ELSE
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                LOOP
                                        ENDIF
                                ENDIF
                        ENDIF
*                                        1            2           3               4          5        6          7             8           9
*                       AADD(m_cax,{dupr->fornec,dupr->tipo,dupr->duplicata,dupr->venc,dupr->valor,mvlr_pg,dupr->comissao,dupr->nota,dupr->vendedor})
                        IF ADIREG()
                                caix-> tipo := m_cax[i,2]
                                caix-> num_dup := m_cax[i,3]
                                caix-> documento := m_cax[i,3]
                                caix-> data := mdatapg
                                caix-> venci := mdatapg
                                caix-> valor := m_cax[i,6]
                                caix-> cod_cli := m_cax[i,1]
*                                caix-> tipo_comp := mtipo_comp
                                caix-> cod_vend := m_cax[i,8]
                                caix-> cod_opera := cod_operado
                                caix-> pg := '*'
                                caix-> op_pg := 'P'
                                caix-> valor_com := m_cax[i,6]
                                caix-> comissao := m_cax[i,7]
                                DBCOMMIT()
                                DBUNLOCK()
                        ELSE
                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                LOOP
                        ENDIF
                NEXT
        ENDIF
        *****************
        SELE(msele);ORDSETFOCUS(morde)
        *****************
        DBCOMMITALL()
        IF ! EMPTY(t_for)
                opcao := mensagem1('Deseja imprimir o Recibo de Pagamento:','S','S,N')
                IF opcao = 'S'
                        rec_dupr(m_pag,m_pos,mquantd_doc,mtot_multa,mtotal,mdatapg,mtp)
                ENDIF
        ENDIF
ENDDO
RETURN NIL

********************************* F I M ********************************
************************************************
*FUNCAO P/IMPRIMIR RECIBO BAIXA DE DOC.RECEBER
************************************************
FUNCTION rec_dupr(m_d,m_p,mqtd,mtot_m,mtot,mdat,mtipo_rel)
********************

LOCAL mtraco := REPLI('=',60),i:=0,mtipo_imp,mextenso,mlinha,mlin,lin
GO m_p[1,1]
SET DEVICE TO PRINT
SET PRINT TO (cfg->prt_ped)
*SET PRINT TO RECDUPR.REL
mtipo_imp := ALLTRIM(cfg->impped)
IF cfg->tipfrmped = 'B'
        mtraco := REPLI('=',60)
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp_resa),38))
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mend_firm),58))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mcid_firm),58))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM('Fone: '+mfone_firm),58))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        imprt(mtipo_imp,'N')
        IF mtipo_rel = NIL
                DEVPOS(PROW()+1,00);DEVOUT(PADC('RECIBO PAGAMENTO',38))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(PADC('DEMONSTRATIVO DE DEBITO',38))
        ENDIF
        imprt(mtipo_imp,'C')
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
                DEVPOS(PROW()+1,00);DEVOUT(SUBSTR(m_d[i],7))
*                DEVPOS(PROW()+1,00);DEVOUT(SUBSTR(m_d[i],7,55))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',60))
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
        DEVPOS(PROW()+3,00);DEVOUT(PADC('----------------------------------------',58))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('       Local e data do recebimento      ',58))
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp_resa))+4),58))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp_resa),58))
        DEVPOS(PROW()+cfg->qtdlinped,00);DEVOUT(CHR(18))
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
ELSEIF cfg->tipfrmped = 'F' .OR. cfg->tipfrmped = 'G'
        mtraco := REPLI('=',80)
        imprt(mtipo_imp,'E')
        DEVPOS(PROW()+1,00);DEVOUT(memp_resa)
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
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp_resa))+4),80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp_resa),80))
        EJECT
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDIF
*lertexto('RECDUPR.REL')
RETURN NIL







