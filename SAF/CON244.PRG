*** CON244.PRG: Consulta de Documentos (Receber)
***
**************************************************

FUNCTION con244(mcli,mcon)
***************
LOCAL MPRG:='CON244'
LOCAL tela,tela1,mtipo,lci,cci,lba,cba,m_dup:={},m_pos:={},point,mdata1,;
      mdata2,mteladata,mtot_dup,mvlr_dup,mtipo_doc,msele,mordem,;
      mdias_atras,mmulta,mjuros,mvlpago,mtot_juros,msub_tot,mtipo_dup,mcartao,;
      mcod_aux,mtot_ind,mvlr_ind,msub_ind,mjuros_ind,mtipo_con,mqtd_dias,mqtd_doc,;
      mtip_pg:='  ',mloc_pg:=' ',marea:=SPACE(15)

PRIVATE mcobra_fin,mtaxa_fin
PRIVATE mcliente,mcgc,mcpf,mcod_cli

IF ! ver_nivel(mprg,'CONTA A RECEBER (CONSULTA DE DOCUMENTO)','1345',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)
lci := 02
cci := 00
lba := 22
cba := 79
set_key('cliente',,,'*')
SET KEY 275 TO cons_doc()
CLEAR GETS
msele := SELE()
morder := INDEXORD()
**************
SELE('dupr');ORDSETFOCUS(4)
***************
WHILE .T.
        mqtd_dias := 0
        mqtd_doc  := 0
        mtipo_dup := ' '
        mtip_pg   := '  '
        mloc_pg   := ' '
        mtot_juros := 0
        msub_tot := 0
        mtot_dup := 0
        mvlr_dup := 0
        mcliente := SPACE(30)
        mcpf := SPACE(11)
        mcgc := SPACE(14)
        mtipo_doc := SPACE(2)
        mcartao := 0
        marea := SPACE(15)
        ASIZE(m_dup,0)
        ASIZE(m_pos,0)
        IF mcli <> NIL
                mcod_cli := VAL(mcli)
        ELSE
                mcod_cli := 0
        ENDIF
        mensagem('[E]mitidos [P]agos A[R]eceber [T]odas [I]ndividual <ESC>p/retorna')
        botao(lci,cci,lba,cba)
        janela(lci,cba,' CONSULTA "Contas a Receber" ')
        @ lci+2,cci+1 TO lci+2,cba-1
        DEVPOS(lci+1,cci+1);DEVOUT('Documento')
        DEVPOS(lci+1,cci+15);DEVOUT('Op')
        DEVPOS(lci+1,cci+18);DEVOUT('Cliente')
        DEVPOS(lci+1,cci+45);DEVOUT('Emissao')
        DEVPOS(lci+1,cci+55);DEVOUT('Venc.')
        DEVPOS(lci+1,cci+63);DEVOUT('Valor')
        DEVPOS(lci+1,cci+74);DEVOUT('Vend.')

        *** RESUMO DA CONSULTA ***
        @ lba-2,cci+1 TO lba-2,cba-1
        DEVPOS(lba-1,02);DEVOUT('Qtd.:')
        DEVPOS(lba-1,14);DEVOUT('Sub Total:')
        DEVPOS(lba-1,38);DEVOUT('Jur+multa:')
        DEVPOS(lba-1,59);DEVOUT('Total:')
        INKEY(0)
        
        IF LASTKEY()=ASC('P') .OR. LASTKEY() = ASC('p')
                janela(lci,cba,' CONSULTA "Contas a Receber" - PAGAS ')
                DEVPOS(lci+1,cci+40);DEVOUT('Dta.Pag.')
                DEVPOS(lci+1,cci+63);DEVOUT('Valor pago')
                mdata1 := mdata_sis -30
                mdata2 := mdata_sis
                mtipo := 'P'
                mtipo_dup := mensagem1('So documentos com juros','N','S,N')
        ELSEIF LASTKEY()=ASC('E') .OR. LASTKEY() = ASC('e')
                janela(lci,cba,' CONSULTA "Contas a Receber" - EMITIDAS ')
                DEVPOS(lci+1,cci+63);DEVOUT('Valor rec.')
                mdata1 := mdata_sis -30
                mdata2 := mdata_sis
                mtipo := 'E'
        ELSEIF LASTKEY() = ASC('C') .OR. LASTKEY() = ASC('c')
                dias_atras(mcod_cli)
                LOOP
        ELSEIF LASTKEY() = ASC('I') .OR. LASTKEY() = ASC('i')
                cons_doc('*')
                LOOP
        ELSEIF LASTKEY() = ASC('R') .OR. LASTKEY() = ASC('r')
                janela(lci,cba,' CONSULTA "Contas a Receber" - A RECEBER ')
                DEVPOS(lci+1,cci+60);DEVOUT('Valor rec.')
                mdata1 := mdata_sis
                mdata2 := mdata_sis + 30
                mtipo := 'R'
        ELSEIF LASTKEY() = ASC('T') .OR. LASTKEY() = ASC('t')
                janela(lci,cba,' CONSULTA "Contas a Receber" - TODAS ')
                limpa(lci+1,cci+1,lci+1,cba-1)
                DEVPOS(lci+1,cci+1);DEVOUT('Documento')
                DEVPOS(lci+1,cci+15);DEVOUT('Cliente')
                DEVPOS(lci+1,cci+37);DEVOUT('Emissao')
                DEVPOS(lci+1,cci+47);DEVOUT('Venc.')
                DEVPOS(lci+1,cci+54);DEVOUT('Dta.Pag.')
                DEVPOS(lci+1,cci+67);DEVOUT('Valor')
                DEVPOS(lci+1,cci+75);DEVOUT('Vend')
                mdata1 := mdata_sis
                mdata2 := mdata_sis + 30
                mtipo := 'T'
        ELSEIF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                SELE(msele);ORDSETFOCUS(morder)
                RETURN NIL
        ELSEIF LASTKEY() <> ASC('P') .AND. LASTKEY() <> ASC('p') .AND. LASTKEY() <> ASC('R') .AND. LASTKEY() <> ASC('r') .AND. LASTKEY() <> ASC('T') .AND. LASTKEY() <> ASC('t')
                LOOP
        ENDIF

        IF mcli = NIL .OR. mtipo = 'E'
                mensagem('Digite o periodo que deseja consultar - <ESC> p/retornar')
                mteladata := SAVESCREEN(00,00,24,79)
                botao(14,10,22,77)
                DEVPOS(15,12);DEVOUT('Cliente.......:')
                DEVPOS(16,12);DEVOUT('Data Inicial..:')
                DEVPOS(17,12);DEVOUT('Data Final....:')
                DEVPOS(18,12);DEVOUT('Tipo Documento:')
                DEVPOS(19,12);DEVOUT('Tipo Operacao.:')
                DEVPOS(20,12);DEVOUT('Local Pagamen.:')
                DEVPOS(21,12);DEVOUT('Area Cliente..:')
                @ 15,28 GET mcod_cli PICT '99999' VALID ver_cli(mcod_cli,15,33) .AND. lim_get() WHEN men_get(16,28,'Informe o Cod.do Cliente - <ENTER>p/todos - <F7>p/pesquisar')
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,mteladata)
                        LOOP
                ENDIF
                ver_cli(mcod_cli,15,33)
                @ 16,28 GET mdata1 VALID IF(EMPTY(mdata1),.F.,.T.)
                @ 17,28 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
                @ 18,28 GET mtipo_doc PICT '@!' VALID mtipo_doc $ 'CH,DU,CT,FI,RE,  ' .AND. lim_get() WHEN men_get(19,33,'Informe o tipo de Documento: <CH>eque, <DU>plicata, <CT>Cartao, <FI>nanciamento, <RE>cebimento Div. ou deixe em branco p/TODOS')
                @ 19,28 GET mtip_pg PICT '@!' VALID lim_get() WHEN men_get(20,33,'Informe o tipo de Operacao que foi feito este PAGAMENTO')
                @ 20,28 GET mloc_pg PICT '@!' VALID mloc_pg $ 'M,C, ' .AND. lim_get() WHEN men_get(21,33,'Digite [M] pelo Movimento [C]aixa ou deixe em branco p/TODOS')
                @ 21,28 GET marea PICT '@!' VALID lim_get() WHEN men_get(22,33,'Digite a AREA desejada ou deixe em branco p/TODOS')
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,mteladata)
                        LOOP
                ENDIF
                IF mtipo_doc = 'CT'
                        mcartao := 0
                        tela1 := SAVESCREEN(00,00,24,79)
                        botao(19,10,21,75)
                        janela(19,51," Vencer/Vencidas ")
                        @ 20,12 SAY "Codigo do Cartao.........:"
                        @ 20,39 GET mcartao PICT '999' VALID ver_cartao(mcartao,20,42) .AND. lim_get() WHEN men_get(21,39,'Informe o codigo do cartao ou deixe em branco para todos')
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(00,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma o Cartao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(00,00,24,79,tela1)
                                LOOP
                        ENDIF
                        RESTSCREEN(00,00,24,79,tela1)
                ENDIF
                IF mtipo_doc = 'FI'
                        mcartao := 0
                        tela1 := SAVESCREEN(01,00,24,79)
                        botao(19,10,21,75)
                        janela(19,51," Vencer/Vencidas ")
                        @ 20,12 SAY "Codigo da Financeira.....:"
                        @ 20,39 GET mcartao PICT '999' VALID IF(EMPTY(mcartao),.T.,ver_finan(mcartao,20,42)) .AND. lim_get() WHEN men_get(21,39,'Informe o codigo da Financeira ou deixe em branco para todos')
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma a Financeira [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        RESTSCREEN(01,00,24,79,tela1)
                ENDIF
                mensagem('Confirma o periodo [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF opcao = 'S'
                        RESTSCREEN(00,00,24,79,mteladata)
                ELSE
                        LOOP
                ENDIF
                SETCOLOR('i')
                DEVPOS(lci,cci+2);DEVOUT('Periodo: '+DTOC(mdata1)+' a '+DTOC(mdata2))
                setcor(1)
                mensagem('Aguarde um momento processando a consulta')
                IF mtipo = 'P'
                        **************
                        SELE('dupr');ORDSETFOCUS(3)
                        GO TOP
                        dupr->(DBSEEK(DTOS(mdata1),.T.))
                        IF EOF()
                                atencao('Nao existe movimento neste periodo !!!')
                                LOOP
                        ENDIF
                        WHILE dupr->datpag <= mdata2 .AND. ! EOF()
                                IF EMPTY(dupr->pago) .OR. EMPTY(dupr->datpag)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mloc_pg) .AND. mloc_pg <> dupr->mov_cx
                                        SKIP
                                        LOOP
                                ENDIF
                                IF dupr->pago = 'T'
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                        SKIP
                                        LOOP
                                ENDIF
                                IF mtipo_dup = 'S' .AND. dupr->juros = 0
                                        SKIP
                                        LOOP
                                ENDIF
*                               mcliente := SPACE(30)
                                ver_cli(VAL(dupr->fornec),,,'*')
                                IF ! EMPTY(marea) .AND. marea <> cli->area
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_pos,RECNO())
                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                +' '+DTOC(dupr->datpag)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->vlpago,'999,999.99');
                                +' '+dupr->vendedor)
                                mtot_juros := mtot_juros + dupr->juros
                                msub_tot := msub_tot + dupr->valor
                                mvlr_dup := mvlr_dup + dupr->vlpago
                                mtot_dup ++
                                SKIP
                        ENDDO
                ELSEIF mtipo = 'R'
                        IF EMPTY(mcod_cli)
                                mtipo_con := mensagem1('Escolha o tipo de consulta [A]NALITICO [S]INTETICO:','A','A,S')
                        ELSE
                                mtipo_con := 'A'
                        ENDIF
                        IF mtipo_con = 'A'
                                **************
                                SELE('dupr');ORDSETFOCUS(4)
                                ***************
                                GO TOP
                                dupr->(DBSEEK(DTOS(mdata1),.T.))

                                IF EOF()
                                        atencao('Nao existe movimento neste periodo !!!')
                                        LOOP
                                ENDIF
                                WHILE dupr->venc <= mdata2 .AND. ! EOF()
                                        IF dupr->pago <> ' '
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                                SKIP
                                                LOOP
                                        ENDIF
                                        ver_cli(VAL(dupr->fornec),,,'*')
                                        IF ! EMPTY(marea) .AND. marea <> cli->area
                                                SKIP
                                                LOOP
                                        ENDIF
                                        AADD(m_pos,RECNO())
                                        AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                        +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                        +' '+dupr->vendedor)
                                        msub_tot := msub_tot + dupr->valor
                                        mmulta := 0
                                        mjuros := 0
                                        mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                        IF mdias_atras > 0
                                                mdias_atras := mdata_sis-dupr->venc
                                                mmulta := dupr->valor * ((set->multa_dup)/100)
                                                mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                        ENDIF
                                        mtot_juros := mtot_juros + mmulta+mjuros
                                        mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                        mtot_dup ++
                                        SKIP
                                ENDDO
                        ELSEIF mtipo_con = 'S'
                                **************
                                SELE('dupr');ORDSETFOCUS(6)
                                ***************
                                GO TOP
                                WHILE ! EOF()
                                        mtot_ind := 0
                                        mvlr_ind := 0
                                        msub_ind := 0
                                        mjuros_ind := 0
                                        mcod_aux := dupr->fornec
                                        WHILE mcod_aux = dupr->fornec .AND. ! EOF()
                                                IF dupr->pago <> ' '
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF dupr->venc > mdata2
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF dupr->venc < mdata1
                                                        SKIP
                                                        LOOP
                                                ENDIF


                                                IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                ver_cli(VAL(dupr->fornec),,,'*')
                                                IF ! EMPTY(marea) .AND. marea <> cli->area
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                AADD(m_pos,RECNO())
                                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                                +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                                +' '+dupr->vendedor)
                                                msub_tot := msub_tot + dupr->valor
                                                mtot_dup ++
                                                msub_ind := msub_ind + dupr->valor
                                                mtot_ind ++
                                                mmulta := 0
                                                mjuros := 0
                                                mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                                IF mdias_atras > 0
                                                        mdias_atras := mdata_sis-dupr->venc
                                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                                ENDIF
                                                mtot_juros := mtot_juros + mmulta+mjuros
                                                mjuros_ind := mjuros_ind + mmulta+mjuros
                                                mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                                mvlr_ind := mvlr_ind + dupr->valor + mmulta + mjuros
                                                SKIP
                                        ENDDO
                                        IF ! EMPTY(mtot_ind)
                                                AADD(m_pos,0)
                                                AADD(m_dup,REPLI('-',77))
                                                AADD(m_pos,0)
                                                AADD(m_dup,'QUANTD: '+STRZERO(mtot_ind,6)+'  SUB-TOTAL:'+TRANSFORM(msub_ind,'999,999.99')+' JURO+MULTA:'+TRANSFORM(mjuros_ind,'999,999.99')+' TOTAL:'+TRANSFORM(mvlr_ind,'9,999,999.99'))
                                                AADD(m_pos,0)
                                                AADD(m_dup,REPLI('-',77))
                                                mtot_ind := 0
                                                mvlr_ind := 0
                                                msub_ind := 0
                                                mjuros_ind := 0
                                        ENDIF
                                ENDDO
                        ENDIF
                ELSEIF mtipo = 'T'
                        IF EMPTY(mcod_cli)
                                mtipo_con := mensagem1('Escolha o tipo de consulta [A]NALITICO [S]INTETICO:','A','A,S')
                        ELSE
                                mtipo_con := 'A'
                        ENDIF
                        IF mtipo_con = 'A'
                                **************
                                SELE('dupr');ORDSETFOCUS(4)
                                ***************
                                GO TOP
                                dupr->(DBSEEK(DTOS(mdata1),.T.))

                                IF EOF()
                                        atencao('Nao existe movimento neste periodo !!!')
                                        LOOP
                                ENDIF
                                WHILE dupr->venc <= mdata2 .AND. ! EOF()
                                        IF dupr->pago = 'T'
                                                SKIP
                                                LOOP

                                        ENDIF
                                        IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                                SKIP
                                                LOOP
                                        ENDIF
                                        ver_cli(VAL(dupr->fornec),,,'*')
                                        IF ! EMPTY(marea) .AND. marea <> cli->area
                                                SKIP
                                                LOOP
                                        ENDIF
                                        AADD(m_pos,RECNO())
                                        AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,10);
                                        +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+DTOC(dupr->datpag)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                        +' '+dupr->vendedor)
                                        msub_tot := msub_tot + dupr->valor
                                        mmulta := 0
                                        mjuros := 0
                                        mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                        IF mdias_atras > 0
                                                mdias_atras := mdata_sis-dupr->venc
                                                mqtd_dias := mqtd_dias + mdias_atras
                                                mqtd_doc ++
                                                mmulta := dupr->valor * ((set->multa_dup)/100)
                                                mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                        ENDIF
                                        mtot_juros := mtot_juros + mmulta+mjuros
                                        mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                        mtot_dup ++

                                        SKIP
                                ENDDO
                        ELSEIF mtipo_con = 'S'
                                **************
                                SELE('dupr');ORDSETFOCUS(6)
                                ***************
                                GO TOP
                                WHILE ! EOF()
                                        mtot_ind := 0
                                        mvlr_ind := 0
                                        msub_ind := 0
                                        mjuros_ind := 0
                                        mcod_aux := dupr->fornec
                                        WHILE mcod_aux = dupr->fornec .AND. ! EOF()
                                                IF dupr->pago = 'T'
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF dupr->venc > mdata2
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF dupr->venc < mdata1
                                                        SKIP
                                                        LOOP
                                                ENDIF


                                                IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                ver_cli(VAL(dupr->fornec),,,'*')
                                                IF ! EMPTY(marea) .AND. marea <> cli->area
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                AADD(m_pos,RECNO())
                                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,10);
                                                +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+DTOC(dupr->datpag)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                                +' '+dupr->vendedor)
                                                msub_tot := msub_tot + dupr->valor
                                                mtot_dup ++
                                                msub_ind := msub_ind + dupr->valor
                                                mtot_ind ++
                                                mmulta := 0
                                                mjuros := 0
                                                mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                                IF mdias_atras > 0
                                                        mdias_atras := mdata_sis-dupr->venc
                                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                                ENDIF
                                                mtot_juros := mtot_juros + mmulta+mjuros
                                                mjuros_ind := mjuros_ind + mmulta+mjuros
                                                mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                                mvlr_ind := mvlr_ind + dupr->valor + mmulta + mjuros
                                                SKIP
                                        ENDDO
                                        IF ! EMPTY(mtot_ind)
                                                AADD(m_pos,0)
                                                AADD(m_dup,REPLI('-',77))
                                                AADD(m_pos,0)
                                                AADD(m_dup,'QUANTD: '+STRZERO(mtot_ind,6)+'  SUB-TOTAL:'+TRANSFORM(msub_ind,'999,999.99')+' JURO+MULTA:'+TRANSFORM(mjuros_ind,'999,999.99')+' TOTAL:'+TRANSFORM(mvlr_ind,'9,999,999.99'))
                                                AADD(m_pos,0)
                                                AADD(m_dup,REPLI('-',77))
                                                mtot_ind := 0
                                                mvlr_ind := 0
                                                msub_ind := 0
                                                mjuros_ind := 0
                                        ENDIF
                                ENDDO
                        ENDIF
                ELSEIF mtipo = 'E'
                        **************
                        SELE('dupr');ORDSETFOCUS(5)
                        ***************
                        GO TOP
                        dupr->(DBSEEK(DTOS(mdata1),.T.))

                        IF EOF()
                                atencao('Nao existe movimento neste periodo !!!')
                                LOOP
                        ENDIF
                        WHILE dupr->emissao <= mdata2 .AND. ! EOF()
                                IF dupr->pago = 'T'
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                        SKIP
                                        LOOP
                                ENDIF
                                ver_cli(VAL(dupr->fornec),,,'*')
                                IF ! EMPTY(marea) .AND. marea <> cli->area
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_pos,RECNO())
                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                +' '+dupr->vendedor)
                                msub_tot := msub_tot + dupr->valor
                                mmulta := 0
                                mjuros := 0
                                mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                IF mdias_atras > 0
                                        mdias_atras := mdata_sis-dupr->venc
                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                ENDIF
                                mtot_juros := mtot_juros + mmulta+mjuros
                                mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                mtot_dup ++
                                SKIP
                        ENDDO
                ENDIF
        ELSE
                mensagem('Aguarde um momento processando a consulta')
                mtipo_doc := 'GE'
                IF mtipo = 'P'
                        **************
                        SELE('dupr');ORDSETFOCUS(6)
                        GO TOP
                        **************
                        IF ! dupr->(DBSEEK(mcli))
                                atencao('Nao existe documento a receber deste cliente')
                                LOOP
                        ENDIF
                        WHILE mcli = dupr->fornec .AND. ! EOF()
                                IF dupr->pago = 'T'
                                        SKIP
                                        LOOP
                                ENDIF
                                IF EMPTY(dupr->pago) .OR. EMPTY(dupr->datpag)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF mtipo_dup = 'S'
                                        IF dupr->juros = 0
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
*                                mcliente := SPACE(30)
                                ver_cli(VAL(dupr->fornec),,,'*')
                                IF ! EMPTY(marea) .AND. marea <> cli->area
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_pos,RECNO())
                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                +' '+DTOC(dupr->datpag)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->vlpago,'999,999.99');
                                +' '+dupr->vendedor)
                                mvlr_dup := mvlr_dup + dupr->vlpago
                                msub_tot := msub_tot + dupr->valor
                                mtot_juros := mtot_juros + dupr->juros
                                mtot_dup ++
                                SKIP
                        ENDDO
                ELSEIF mtipo = 'T'
                        **************
                        SELE('dupr');ORDSETFOCUS(6)
                        GO TOP
                        ***************
                        IF ! dupr->(DBSEEK(mcli))
                                atencao('Nao existe documento a receber deste cliente')
                                LOOP
                        ENDIF
                        WHILE mcli = dupr->fornec .AND. ! EOF()
                                IF dupr->pago = 'T'
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_pos,RECNO())
                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,10);
                                +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+DTOC(dupr->datpag)+' '+TRANSFORM(dupr->valor,'9,999,999.99');
                                +' '+dupr->vendedor)
                                msub_tot := msub_tot + dupr->valor
                                mmulta := 0
                                mjuros := 0
                                mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                IF ! EMPTY(dupr->datpag)
                                        IF  dupr->datpag - dupr->venc > 0
                                                mqtd_dias := mqtd_dias + dupr->datpag - dupr->venc
                                                mqtd_doc ++
                                        ENDIF
                                ELSEIF mdias_atras > 0
                                        mdias_atras := mdata_sis-dupr->venc
                                        mqtd_dias := mqtd_dias + mdias_atras
                                        mqtd_doc ++
                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                ENDIF
                                mtot_juros := mtot_juros + mmulta+mjuros
                                mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                mtot_dup ++
                                SKIP
                        ENDDO
                ELSEIF mtipo = 'R'
                        **************
                        SELE('dupr');ORDSETFOCUS(6)
                        ***************
                        GO TOP
                        IF ! dupr->(DBSEEK(mcli))
                                atencao('Nao existe documento a receber deste cliente')
                                LOOP
                        ENDIF
                        WHILE mcli = dupr->fornec .AND. ! EOF()
                                IF dupr->pago <> ' '
                                        SKIP
                                        LOOP
                                ENDIF
                                mcliente := SPACE(30)
                                ver_cli(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,dupr->tipo+dupr->duplicata+dupr->conta+' '+dupr->tip_pg+' '+dupr->fornec+'-'+LEFT(dupr->cliente,16);
                                +' '+DTOC(dupr->emissao)+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'999,999.99');
                                +' '+dupr->vendedor)
                                msub_tot := msub_tot + dupr->valor
                                mtot_dup ++
                                mmulta := 0
                                mjuros := 0
                                mdias_atras := mdata_sis-(dupr->venc+set->dia_venc)
                                IF mdias_atras > 0
                                        mdias_atras := mdata_sis-dupr->venc
                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                ENDIF
                                mtot_juros := mtot_juros + mmulta+mjuros
                                mvlr_dup := mvlr_dup + dupr->valor + mmulta + mjuros
                                SKIP
                        ENDDO
                ENDIF
        ENDIF
        mensagem('Aguarde um momento processando a consulta')
        IF LEN(m_dup) = 0
                atencao('Nao existe movimento neste periodo !!!')
                LOOP
        ENDIF
        WHILE .T.
                setcor(3)
                DEVPOS(lba-1,07);DEVOUT(STRZERO(mtot_dup,6))
                DEVPOS(lba-1,24);DEVOUT(TRANSFORM(msub_tot,'99,999,999.99'))
                DEVPOS(lba-1,48);DEVOUT(TRANSFORM(mtot_juros,'999,999.99'))
                DEVPOS(lba-1,65);DEVOUT(TRANSFORM(mvlr_dup,'99,999,999.99'))
*               DEVPOS(lba,cci+1);DEVOUT('Media Dias de Atraso: '+TRANSFORM(mqtd_dias / mqtd_doc,'99,999.99')+' - Qtd.de Documentos: '+TRANSFORM(mqtd_doc,'99,999.99'))
                setcor(1)
                CLEAR TYPEAHEAD
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
                point := ACHOICE(lci+3,cci+1,lba-3,cba-1,m_dup,,,point)

                DO CASE
                        CASE LASTKEY()=27
                                IF LASTKEY() = 27
                                        IF mcli = NIL
*                                               SETMODE(25,80)
                                        ENDIF
*                                       SETMODE(25,80)
                                        RESTSCREEN(00,00,24,79,tela)
                                        SELE(msele);ORDSETFOCUS(morder)
                                        RETURN NIL
                                ENDIF
                                EXIT
                        CASE LASTKEY() = 13
                                IF m_pos[point] = 0
                                        LOOP
                                ENDIF

                                GO m_pos[point]
                                mdias_atras := 0
                                mmulta := 0
                                mjuros := 0
                                mvlpago := 0
                                mdias_atras := mdata_sis-dupr->venc
                                IF mdias_atras > 0
                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                ENDIF
                                mvlpago=valor+mjuros+mmulta
                                tela1 := SAVESCREEN(00,00,24,79)
                                botao(04,00,22,79)
                                DEVPOS(05,01);DEVOUT('Cliente.............:')
                                DEVPOS(06,01);DEVOUT('Banco...............:')
                                DEVPOS(06,27);DEVOUT('Agencia:')
                                DEVPOS(06,51);DEVOUT('C/C:')
                                DEVPOS(07,01);DEVOUT('No.da Documento ....:')
                                DEVPOS(07,40);DEVOUT('Tipo Operacao:')
                                DEVPOS(08,01);DEVOUT('Data Emissao........:')
                                DEVPOS(09,01);DEVOUT('Data Vencimento.....:')
                                DEVPOS(10,01);DEVOUT('Vlr.Bruto Documento.:')
                                DEVPOS(11,01);DEVOUT('Vlr. Documento ...R$:')


                                DEVPOS(18,01);DEVOUT('No.do Pedido........:')
                                DEVPOS(19,01);DEVOUT('Operador............:')
                                DEVPOS(20,01);DEVOUT('Correntista.........:')
                                DEVPOS(21,01);DEVOUT('OBS.:')

                                setcor(3)
                                DEVPOS(05,23);DEVOUT(dupr->fornec+'-'+dupr->cliente)
                                DEVPOS(06,21);DEVOUT(dupr->numero)
                                DEVPOS(06,36);DEVOUT(dupr->agencia)
                                DEVPOS(06,56);DEVOUT(dupr->c_c)
                                DEVPOS(07,23);DEVOUT(dupr->tipo+dupr->duplicata)
                                DEVPOS(07,55);DEVOUT(dupr->tip_pg)
                                DEVPOS(08,23);DEVOUT(DTOC(dupr->emissao))
                                DEVPOS(09,23);DEVOUT(DTOC(dupr->venc))
                                DEVPOS(10,23);DEVOUT(TRANSFORM(dupr->valor_dup,'9,999,999.99'))
                                DEVPOS(11,23);DEVOUT(TRANSFORM(dupr->valor,'9,999,999.99'))
                                DEVPOS(11,COL()+1);DEVOUT(dupr->conta)

                                DEVPOS(18,23);DEVOUT(dupr->num_ped)
                                DEVPOS(19,23);DEVOUT(dupr->operador)
                                DEVPOS(20,23);DEVOUT(dupr->corrente)
                                DEVPOS(21,07);DEVOUT(dupr->obs)

                                IF ! EMPTY(dupr->datpag) .OR. dupr->pago <> ' '
                                        setcor(1)
                                        DEVPOS(12,01);DEVOUT('Data Pagamento .....:')
                                        DEVPOS(13,01);DEVOUT('Pago efetuado Com...:')
                                        DEVPOS(14,01);DEVOUT('Juros Pago .......R$:')
                                        DEVPOS(15,01);DEVOUT('Desconto .........R$:')
                                        DEVPOS(16,01);DEVOUT('Valor Pago .......R$:')
                                        DEVPOS(16,37);DEVOUT('Tipo Baixa:')
                                        DEVPOS(17,01);DEVOUT('Local Pagamento.....:')
                                        setcor(3)
                                        DEVPOS(12,23);DEVOUT(DTOC(dupr->datpag))
                                        DEVPOS(13,23);DEVOUT(STRZERO(dupr->datpag-dupr->venc,3)+' Dias.')
                                        DEVPOS(14,23);DEVOUT(TRANSFORM(dupr->juros,'9,999,999.99'))
                                        DEVPOS(15,23);DEVOUT(TRANSFORM(dupr->desc,'9,999,999.99'))
                                        DEVPOS(16,23);DEVOUT(TRANSFORM(dupr->vlpago,'9,999,999.99'))
                                        IF dupr->mov_cx = 'C'
                                                DEVPOS(16,48);DEVOUT('CAIXA')
                                        ELSEIF dupr->mov_cx = 'M'
                                                DEVPOS(16,48);DEVOUT('MOVIMENTO')
                                        ENDIF
                                        DEVPOS(17,23);DEVOUT(dupr->banco)
                                ELSE
                                        setcor(1)
                                        DEVPOS(12,01);DEVOUT('Prazo estipulado....:')
                                        DEVPOS(14,01);DEVOUT('Vlr. Multa+Juros..R$:')
                                        DEVPOS(15,01);DEVOUT('Vlr. a Pagar......R$:')
                                        DEVPOS(16,01);DEVOUT('Local Pagamento.....:')
                                        IF mdata_sis<venc
                                                setcor(1)
                                                DEVPOS(13,01);DEVOUT('Faltam p/ Vencimento:')
                                                setcor(3)
                                                DEVPOS(13,23);DEVOUT(STRZERO(dupr->venc-mdata_sis,3)+' Dias.')
                                        ELSE
                                                setcor(1)
                                                DEVPOS(13,01);DEVOUT('Atrazo do vencimento:')
                                                setcor(3)
                                                DEVPOS(13,23);DEVOUT(STRZERO(mdata_sis-dupr->venc,3)+' Dias.')
                                        ENDIF
                                        setcor(3)
                                        DEVPOS(12,23);DEVOUT(STRZERO(dupr->venc-dupr->emissao,3)+' Dias.')
                                        DEVPOS(14,23);DEVOUT(TRANSFORM(mmulta+mjuros,'9,999,999.99'))
                                        DEVPOS(15,23);DEVOUT(TRANSFORM(mvlpago,'9,999,999.99'))
                                        DEVPOS(16,23);DEVOUT(dupr->banco)
                                ENDIF
                                setcor(1)
                                mensagem('Pressione qualquer tecla p/retornar')
                                INKEY(0)
                                RESTSCREEN(00,00,24,79,tela1)
                                LOOP
                ENDCASE
        ENDDO
ENDDO
******************************* FIM **************************************8


