*** CON214.PRG: Consulta de Duplicatas (A pagar)
***
**************************************************

FUNCTION con214(mfor)
***************
LOCAL MPRG:='CON214'
LOCAL tela,tela1,lci,cci,lba,cba,m_dup:={},m_pos:={},point,tipo,mdata1,;
      mdata2,mteladata,mtot_dup,mvlr_dup,mcod_for       //,mtip_for
LOCAL mfo,mfv,mvv,mad,mtr,mfi,mdv

PRIVATE mfornece,muf

IF ! ver_nivel(mprg,'CONSULTA DE DOCUMENTOS (C.APAGAR)','15')
        RETURN NIL
ENDIF

lci := 06
cci := 00
lba := 19
cba := 79

tela := SAVESCREEN(lci,cci-1,lba+4,cba)

**************
SELE('dupp');ORDSETFOCUS(4)
***************
WHILE .T.
        mfo := SPACE(1)
        mfv := SPACE(1)
        mvv := SPACE(1)
        mad := SPACE(1)
        mtr := SPACE(1)
        mfi := SPACE(1)
        mdv := SPACE(1)
*       mtip_for := SPACE(2)
        mtot_dup := 0
        mvlr_dup := 0
        mdata1 := mdata_sis
        mdata2 := mdata_sis +30
        mcliente := SPACE(30)
        mcpf := SPACE(11)
        mcgc := SPACE(14)
        ASIZE(m_dup,0)
        ASIZE(m_pos,0)
        mensagem('[R]=Doc.Recebida [P]=Doc.PAGAS  [A]=Doc.A Pagar  <ESC>retornar')
        botao(lci,cci,lba,cba)
        janela(lci,cba,' CONSULTA "Contas a Pagar" ')
        @ lci+2,cci+1 TO lci+2,cba-1
        DEVPOS(lci+1,cci+3);DEVOUT('Duplicata')
        DEVPOS(lci+1,cci+16);DEVOUT('Fornecedor')
        DEVPOS(lci+1,cci+42);DEVOUT('Emissao')
        DEVPOS(lci+1,cci+52);DEVOUT('Venc.')
        DEVPOS(lci+1,cci+68);DEVOUT('Valor')
*       DEVPOS(lci+1,cci+75);DEVOUT('Vend')
        *** RESUMO DA CONSULTA ***
        botao(20,00,22,79)
        DEVPOS(21,02);DEVOUT('Quantidade de Duplicatas:')
        DEVPOS(21,36);DEVOUT('Total de Duplicatas R$:')
        INKEY(0)
        
        IF LASTKEY()=ASC('P') .OR. LASTKEY() = ASC('p')
                janela(lci,cba,' CONSULTA "Contas a Pagar" - Doc.PAGAS ')
                DEVPOS(lci+1,cci+42);DEVOUT('Dta.Pag.')
                DEVPOS(lci+1,cci+63);DEVOUT('Valor pago')
                tipo := 'P'
                mdata1 := mdata_sis - 30
                mdata2 := mdata_sis
        ELSEIF LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a')
                janela(lci,cba,' CONSULTA "Contas a Pagar" - Doc.A PAGAR ')
                DEVPOS(lci+1,cci+60);DEVOUT('Valor a pagar')
                tipo := 'A'
                mdata1 := mdata_sis
                mdata2 := mdata_sis +30
        ELSEIF LASTKEY() = ASC('R') .OR. LASTKEY() = ASC('r')
                janela(lci,cba,' CONSULTA "Contas a Pagar" - Doc.RECEBIDOS ')
                DEVPOS(lci+1,cci+60);DEVOUT('Valor a pagar')
                tipo := 'R'
                mdata1        := CTOD('01/'+SUBSTR(DTOC(mdata_sis),4,5))
                mdata2        := CTOD('30/'+SUBSTR(DTOC(mdata_sis),4,5))
        ELSEIF LASTKEY() = 27
                RESTSCREEN(lci,cci-1,lba+4,cba,tela)
                RETURN NIL
        ELSEIF LASTKEY() <> ASC('P') .AND. LASTKEY() <> ASC('p') .AND. LASTKEY() <> ASC('A') .AND. LASTKEY() <> ASC('a') .AND. LASTKEY() <> ASC('R') .AND. LASTKEY() <> ASC('r')
                LOOP
        ENDIF
        IF mfor = NIL
                mcod_for := 0
                mensagem('Digite o periodo que deseja consultar - <ESC> p/retornar')
                mteladata := SAVESCREEN(00,00,24,79)
                botao(14,09,19,77)
                DEVPOS(15,10);DEVOUT('Fornecedor..:')
                DEVPOS(16,10);DEVOUT('Data Inicial:')
                DEVPOS(17,10);DEVOUT('Data Final..:')
                DEVPOS(18,10);DEVOUT('Tipo Conta..: FO:    FV:    VV:    TR:    AD:    FI:    DV:')
                @ 15,24 GET mcod_for PICT '9999' VALID v_fornece(mcod_for,15,36)
                @ 16,24 GET mdata1 VALID IF(EMPTY(mdata1),.F.,.T.)
                @ 17,24 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
*               @ 18,24 GET mtip_for PICT '@!' VALID mtip_for $ '  ,FO,FV,VV,AD,TR,FI' .AND. lim_get() WHEN men_get(19,31,'Informe o Tipo de Conta: <FO>rnecedor  <FV>Fixas Venda  <VV>Variavel Venda  <AD>ministrativa  <TR>ibutaria  <FI>nanceira  <NAO informe p/TODOS>')
                @ 18,28 GET mfo PICT '@!' VALID mfo $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,35,'Marque com um "X" p/consulta Conta de Fornecedor ou deixe em branco')
                @ 18,35 GET mfv PICT '@!' VALID mfv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,42,'Marque com um "X" p/consulta Conta de Fixa de Venda ou deixe em branco')
                @ 18,42 GET mvv PICT '@!' VALID mvv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,49,'Marque com um "X" p/consulta Conta de Variavel de Venda ou deixe em branco')
                @ 18,49 GET mtr PICT '@!' VALID mtr $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,56,'Marque com um "X" p/consulta Conta de Tributaria ou deixe em branco')
                @ 18,56 GET mad PICT '@!' VALID mad $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,63,'Marque com um "X" p/consulta Conta de Administrativa ou deixe em branco')
                @ 18,63 GET mfi PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
                @ 18,70 GET mdv PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,mteladata)
                        LOOP
                ENDIF
                IF EMPTY(mfo+mfv+mvv+mtr+mad+mfi+mdv) .AND. EMPTY(mcod_for)
                        atencao('Nao e possivel fazer esta consulta sem marcar algum tipo de CONTA')
                        LOOP
                ENDIF
                mensagem('Confirma o periodo [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N';LOOP;ENDIF
                RESTSCREEN(00,00,24,79,mteladata)
                mensagem('Aguarde um momento processando a consulta')
                IF tipo = 'A'
                        **************
                        SELE('dupp');ORDSETFOCUS(4)
                        GO TOP
                        ***************
                        dupp->(DBSEEK(DTOS(mdata1),.T.))
                        IF EOF()
                                atencao('Nao existe movimento neste periodo !!!')
                                LOOP
                        ENDIF
                        WHILE dupp->venc <= mdata2 .AND. ! EOF()
                                IF ! EMPTY(dupp->pago)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcod_for)
                                        IF STRZERO(mcod_for,4) <> dupp->fornec
                                                SKIP
                                                LOOP
                                        ENDIF
                                ELSE
                                        WHILE dupp->venc <= mdata2 .AND. ! EOF() .AND. EMPTY(mcod_for)
                                                IF ! EMPTY(dupp->pago)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF mfo = 'X' .AND. 'FO' = tip_for
                                                        EXIT
                                                ELSEIF mfv = 'X' .AND. 'FV' = tip_for
                                                        EXIT
                                                ELSEIF mvv = 'X' .AND. 'VV' = tip_for
                                                        EXIT
                                                ELSEIF mtr = 'X' .AND. 'TR' = tip_for
                                                        EXIT
                                                ELSEIF mad = 'X' .AND. 'AD' = tip_for
                                                        EXIT
                                                ELSEIF mfi = 'X' .AND. 'FI' = tip_for
                                                        EXIT
                                                ELSEIF mdv = 'X' .AND. 'DV' = tip_for
                                                        EXIT
                                                ELSE
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDDO
                                ENDIF
                                IF EOF() .OR.  dupp->venc > mdata2
                                        EXIT
                                ENDIF
*                               mfornece := SPACE(40)
*                               v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->emissao)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->valor,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->valor
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ELSEIF tipo = 'P'
                        **************
                        SELE('dupp');ORDSETFOCUS(3)
                        ***************
                        dupp->(DBSEEK(DTOS(mdata1),.T.))
                        IF EOF()
                                atencao('Nao existe movimento neste periodo !!!')
                                LOOP
                        ENDIF
                        WHILE dupp->datpag <= mdata2 .AND. ! EOF()
                                IF EMPTY(dupp->pago)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mcod_for)
                                        IF STRZERO(mcod_for,4) <> dupp->fornec
                                                SKIP
                                                LOOP
                                        ENDIF
                                ELSE
                                        WHILE dupp->datpag <= mdata2 .AND. ! EOF() .AND. EMPTY(mcod_for)
                                                IF EMPTY(dupp->pago)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF mfo = 'X' .AND. 'FO' = tip_for
                                                        EXIT
                                                ELSEIF mfv = 'X' .AND. 'FV' = tip_for
                                                        EXIT
                                                ELSEIF mvv = 'X' .AND. 'VV' = tip_for
                                                        EXIT
                                                ELSEIF mtr = 'X' .AND. 'TR' = tip_for
                                                        EXIT
                                                ELSEIF mad = 'X' .AND. 'AD' = tip_for
                                                        EXIT
                                                ELSEIF mfi = 'X' .AND. 'FI' = tip_for
                                                        EXIT
                                                ELSEIF mdv = 'X' .AND. 'DV' = tip_for
                                                        EXIT
                                                ELSE
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDDO
                                ENDIF
                                IF EOF() .OR.  dupp->venc > mdata2
                                        EXIT
                                ENDIF
*                               mfornece := SPACE(30)
*                               v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->datpag)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->vlpago,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->vlpago
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ELSE
                        **************
                        SELE('dupp');ORDSETFOCUS(5)
                        GO TOP
                        ***************
                        dupp->(DBSEEK(DTOS(mdata1),.T.))
                        IF EOF()
                                atencao('Nao existe movimento neste periodo !!!')
                                LOOP
                        ENDIF
                        WHILE dupp->emissao <= mdata2 .AND. ! EOF()
                                IF ! EMPTY(mcod_for)
                                        IF STRZERO(mcod_for,4) <> dupp->fornec
                                                SKIP
                                                LOOP
                                        ENDIF
                                ELSE
                                        WHILE dupp->emissao <= mdata2 .AND. ! EOF() .AND. EMPTY(mcod_for)
                                                IF mfo = 'X' .AND. 'FO' = tip_for
                                                        EXIT
                                                ELSEIF mfv = 'X' .AND. 'FV' = tip_for
                                                        EXIT
                                                ELSEIF mvv = 'X' .AND. 'VV' = tip_for
                                                        EXIT
                                                ELSEIF mtr = 'X' .AND. 'TR' = tip_for
                                                        EXIT
                                                ELSEIF mad = 'X' .AND. 'AD' = tip_for
                                                        EXIT
                                                ELSEIF mfi = 'X' .AND. 'FI' = tip_for
                                                        EXIT
                                                ELSEIF mdv = 'X' .AND. 'DV' = tip_for
                                                        EXIT
                                                ELSE
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDDO
                                ENDIF
                                IF EOF() .OR.  dupp->venc > mdata2
                                        EXIT
                                ENDIF
*                                mfornece := SPACE(30)
*                                v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->emissao)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->valor,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->valor
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ENDIF
        ELSE
                mensagem('Aguarde um momento processando a consulta')
                IF tipo = 'A'
                        **************
                        SELE('dupp');ORDSETFOCUS(4)
                        ***************
                        GO TOP
                        WHILE ! EOF()
                                IF ! EMPTY(dupp->pago) .OR. dupp->fornec <> mfor
                                        SKIP
                                        LOOP
                                ENDIF
*                                mfornece := SPACE(30)
*                                v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->emissao)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->valor,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->valor
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ELSEIF tipo = 'P'
                        **************
                        SELE('dupp');ORDSETFOCUS(3)
                        ***************
                        GO TOP
                        WHILE ! EOF()
                                IF EMPTY(dupp->pago) .OR. dupp->fornec <> mfor
                                        SKIP
                                        LOOP
                                ENDIF
*                                mfornece := SPACE(30)
*                                v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->datpag)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->vlpago,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->vlpago
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ELSE
                        **************
                        SELE('dupp');ORDSETFOCUS(5)
                        ***************
                        GO TOP
                        WHILE ! EOF()
                                IF dupp->fornec <> mfor
                                        SKIP
                                        LOOP
                                ENDIF
*                                mfornece := SPACE(30)
*                                v_fornece(VAL(fornec),,,'*')
                                AADD(m_pos,RECNO())
                                AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+dupp->conta+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                                +'  '+DTOC(dupp->datpag)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->vlpago,'9,999,999.99'))
                                mvlr_dup := mvlr_dup + dupp->vlpago
                                mtot_dup ++
                                SKIP +1
                        ENDDO
                ENDIF
        ENDIF
        IF LASTKEY() = 27;LOOP;ENDIF

        SETCOLOR('i')
        DEVPOS(lci,cci+2);DEVOUT('Periodo: '+DTOC(mdata1)+' a '+DTOC(mdata2))
        setcor(1)
        mensagem('Aguarde um momento processando a consulta')
        IF LEN(m_dup) = 0
                atencao('Nao existe movimento neste periodo !!!')
                LOOP
        ENDIF
        WHILE .T.
                setcor(3)
                DEVPOS(21,28);DEVOUT(STRZERO(mtot_dup,6))
                DEVPOS(21,60);DEVOUT(TRANSFORM(mvlr_dup,'999,999,999.99'))
                setcor(1)
                CLEAR TYPEAHEAD
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
                point := ACHOICE(lci+3,cci+1,lba-1,cba-1,m_dup)

                DO CASE
                        CASE LASTKEY()=27
                                EXIT
                        CASE LASTKEY() = 13
                                GO m_pos[point]
*                               ver_ven(cod_vend)
                                tela1 := SAVESCREEN(08,04,22,75)
                                IF tipo = 'P'
                                        botao(08,06,21,75)
                                        DEVPOS(09,08);DEVOUT('Cliente.............:')
                                        setcor(3)
                                        DEVPOS(09,COL()+1);DEVOUT(dupp->fornec+'-'+dupp->cliente)
                                        setcor(1)
                                        DEVPOS(10,08);DEVOUT('Banco...............:')
                                        setcor(3)
                                        DEVPOS(10,COL()+1);DEVOUT(dupp->numero)
                                        setcor(1)
                                        DEVPOS(11,08);DEVOUT('No.da Duplicata ....:')
                                        setcor(3)
                                        DEVPOS(11,COL()+1);DEVOUT(dupp->tipo+dupp->duplicata)
                                        setcor(1)
                                        DEVPOS(11,COL()+2);DEVOUT('Tipo Conta:')
                                        setcor(3)
                                        DEVPOS(11,COL()+1);DEVOUT(dupp->tip_for)
                                        setcor(1)
                                        DEVPOS(12,08);DEVOUT('Vlr. Duplicata ...R$:')
                                        setcor(3)
                                        DEVPOS(12,COL()+1);DEVOUT(TRANSFORM(dupp->valor,'9,999,999.99'))
                                        setcor(1)
                                        DEVPOS(13,08);DEVOUT('Juros Pago .......R$:')
                                        setcor(3)
                                        DEVPOS(13,COL()+1);DEVOUT(TRANSFORM(dupp->juros,'9,999,999.99'))
                                        setcor(1)
                                        DEVPOS(14,08);DEVOUT('Desconto .........R$:')
                                        setcor(3)
                                        DEVPOS(14,COL()+1);DEVOUT(TRANSFORM(dupp->desc,'9,999,999.99'))
                                        setcor(1)
                                        DEVPOS(15,08);DEVOUT('Valor Pago .......R$:')
                                        setcor(3)
                                        DEVPOS(15,COL()+1);DEVOUT(TRANSFORM(dupp->vlpago,'9,999,999.99'))
                                        setcor(1)
                                        DEVPOS(16,08);DEVOUT('Data Vencimento.....:')
                                        setcor(3)
                                        DEVPOS(16,COL()+1);DEVOUT(DTOC(dupp->venc))
                                        setcor(1)
                                        DEVPOS(17,08);DEVOUT('Data Pagamento .....:')
                                        setcor(3)
                                        DEVPOS(17,COL()+1);DEVOUT(DTOC(dupp->datpag))
                                        setcor(1)
                                        DEVPOS(18,08);DEVOUT('Pago efetuado Com...:')
                                        setcor(3)
                                        DEVPOS(18,COL()+1);DEVOUT(STRZERO(dupp->datpag-dupp->venc,3)+' Dias.')
                                        setcor(1)
                                        DEVPOS(19,08);DEVOUT('No.do Pedido........:')
                                        setcor(3)
                                        DEVPOS(19,COL()+1);DEVOUT(dupp->num_ped)
                                        setcor(1)
                                        DEVPOS(20,08);DEVOUT('Operador............:')
                                        setcor(3)
                                        DEVPOS(20,COL()+1);DEVOUT(dupp->operador)
                                        setcor(1)
                                ELSE
                                        botao(09,06,20,75)
                                        DEVPOS(10,08);DEVOUT('Cliente.............:')
                                        setcor(3)
                                        DEVPOS(10,COL()+1);DEVOUT(dupp->fornec+'-'+dupp->cliente)
                                        setcor(1)
                                        DEVPOS(11,08);DEVOUT('Banco...............:')
                                        setcor(3)
                                        DEVPOS(11,COL()+1);DEVOUT(dupp->numero)
                                        setcor(1)
                                        DEVPOS(12,08);DEVOUT('No.da Duplicata ....:')
                                        setcor(3)
                                        DEVPOS(12,COL()+1);DEVOUT(dupp->tipo+dupp->duplicata)
                                        setcor(1)
                                        DEVPOS(12,COL()+2);DEVOUT('Tipo Conta:')
                                        setcor(3)
                                        DEVPOS(12,COL()+1);DEVOUT(dupp->tip_for)
                                        setcor(1)
                                        DEVPOS(13,08);DEVOUT('Vlr. Duplicata ...R$:')
                                        setcor(3)
                                        DEVPOS(13,COL()+1);DEVOUT(TRANSFORM(dupp->valor,'9,999,999.99'))
                                        setcor(1)
                                        DEVPOS(14,08);DEVOUT('Data Emissao........:')
                                        setcor(3)
                                        DEVPOS(14,COL()+1);DEVOUT(DTOC(dupp->emissao))
                                        setcor(1)
                                        DEVPOS(15,08);DEVOUT('Data Vencimento.....:')
                                        setcor(3)
                                        DEVPOS(15,COL()+1);DEVOUT(DTOC(dupp->venc))
                                        setcor(1)
                                        DEVPOS(16,08);DEVOUT('Prazo estipulado....:')
                                        setcor(3)
                                        DEVPOS(16,COL()+1);DEVOUT(STRZERO(dupp->venc-dupp->emissao,3)+' Dias.')
                                        setcor(1)
                                        IF mdata_sis<venc
                                                DEVPOS(17,08);DEVOUT('Faltam p/ Vencimento:')
                                                setcor(3)
                                                DEVPOS(17,COL()+1);DEVOUT(STRZERO(dupp->venc-mdata_sis,3)+' Dias.')
                                                setcor(1)
                                        ELSE
                                                DEVPOS(17,08);DEVOUT('Atrazo do vencimento:')
                                                setcor(3)
                                                DEVPOS(17,COL()+1);DEVOUT(STRZERO(mdata_sis-dupp->venc,3)+' Dias.')
                                                setcor(1)
                                        ENDIF
                                        setcor(1)
                                        DEVPOS(18,08);DEVOUT('No.do Pedido........:')
                                        setcor(3)
                                        DEVPOS(18,COL()+1);DEVOUT(dupp->num_ped)
                                        setcor(1)
                                        DEVPOS(19,08);DEVOUT('Operador............:')
                                        setcor(3)
                                        DEVPOS(19,COL()+1);DEVOUT(dupp->operador)
                                        setcor(1)
                                ENDIF
                                mensagem('Pressione qualquer tecla p/retornar')
                                INKEY(0)
                                RESTSCREEN(08,04,22,75,tela1)
                                LOOP
                ENDCASE
        ENDDO
ENDDO
******************************* FIM *****************************************
*** CON215.PRG: Estorno de Duplicatas (A pagar)
***
**************************************************

FUNCTION con215()
***************
LOCAL mprg:='CON215'
LOCAL tela,tela1,lci,cci,lba,cba,m_dup:={},m_pos:={},point,tipo,mdata1,;
      mdata2,mteladata,mtot_dup,mvlr_dup

PRIVATE mfornece,muf

IF ! ver_nivel(mprg,'ESTORNO DE DOCUMENTO (C.APAGAR)','15')
        RETURN NIL
ENDIF
lci := 06
cci := 00
lba := 19
cba := 79

tela := SAVESCREEN(lci,cci-1,lba+4,cba)

**************
SELE('dupp');ORDSETFOCUS(4)
***************
WHILE .T.
        mtot_dup := 0
        mvlr_dup := 0
        mdata1 := mdata_sis
        mdata2 := mdata_sis +30
        mcliente := SPACE(30)
        mcpf := SPACE(11)
        mcgc := SPACE(14)
        ASIZE(m_dup,0)
        ASIZE(m_pos,0)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' ESTORNO "Contas a Pagar" - Dup.PAGAS ')
        @ lci+2,cci+1 TO lci+2,cba-1
        DEVPOS(lci+1,cci+3);DEVOUT('Duplicata')
        DEVPOS(lci+1,cci+16);DEVOUT('Fornecedor')
        DEVPOS(lci+1,cci+42);DEVOUT('Dta.Pag.')
        DEVPOS(lci+1,cci+52);DEVOUT('Venc.')
        DEVPOS(lci+1,cci+63);DEVOUT('Valor pago')
*       DEVPOS(lci+1,cci+75);DEVOUT('Vend')
        *** RESUMO DA CONSULTA ***
        botao(20,00,22,79)
        DEVPOS(21,02);DEVOUT('Quantidade de Duplicatas:')
        DEVPOS(21,36);DEVOUT('Total de Duplicatas R$:')
        tipo := 'P'
        mdata1 := mdata_sis - 30
        mdata2 := mdata_sis
        IF LASTKEY() = 27
                RESTSCREEN(lci,cci-1,lba+4,cba,tela)
                RETURN NIL
        ENDIF
        mensagem('Digite o periodo que deseja consultar - <ESC> p/retornar')
        mteladata := SAVESCREEN(15,23,19,50)
        botao(15,25,18,50)
        DEVPOS(16,27);DEVOUT('Data Inicial:')
        DEVPOS(17,27);DEVOUT('Data Final..:')
        @ 16,41 GET mdata1
        @ 17,41 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(15,23,19,50,mteladata)
                LOOP
        ENDIF
        IF EMPTY(mdata1)
                LOOP
        ENDIF
        mensagem('Confirma o periodo [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'N';LOOP;ENDIF
        RESTSCREEN(15,23,19,50,mteladata)
        WHILE .T.
                limpa(lci+3,cci+1,lba-1,cba-1)
                ASIZE(m_pos,0)
                ASIZE(m_dup,0)
                mvlr_dup := 0
                mtot_dup := 0

                mensagem('Aguarde um momento processando a consulta')
                **************
                SELE('dupp');ORDSETFOCUS(3)
                GO TOP
                ***************
                dupp->(DBSEEK(DTOS(mdata1),.T.))

                IF EOF()
                        atencao('Nao existe movimento neste periodo !!!')
                        LOOP
                ENDIF
                WHILE datpag <= mdata2 .AND. ! EOF()
                        IF dupp->pago = ' '
                                SKIP
                                LOOP
                        ENDIF
*                        mfornece := SPACE(30)
*                        v_fornece(VAL(fornec),,,'*')
                        AADD(m_pos,RECNO())
                        AADD(m_dup,'  '+dupp->tipo+dupp->duplicata+' '+dupp->fornec+'-'+LEFT(dupp->cliente,20);
                        +'  '+DTOC(dupp->datpag)+'  '+DTOC(dupp->venc)+'  '+TRANSFORM(dupp->vlpago,'9,999,999.99'))
                        mvlr_dup := mvlr_dup + dupp->vlpago
                        mtot_dup ++
                        SKIP
                ENDDO
                SETCOLOR('i')
                DEVPOS(lci,cci+2);DEVOUT('Periodo: '+DTOC(mdata1)+' a '+DTOC(mdata2))
                setcor(1)
                mensagem('Aguarde um momento processando a consulta')
                IF LEN(m_dup) = 0
                        atencao('Nao existe movimento neste periodo !!!')
                        EXIT
                ENDIF
                setcor(3)
                DEVPOS(21,28);DEVOUT(STRZERO(mtot_dup,6))
                DEVPOS(21,60);DEVOUT(TRANSFORM(mvlr_dup,'999,999,999.99'))
                setcor(1)
                CLEAR TYPEAHEAD
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER>p/Estornar" )
                point := ACHOICE(lci+3,cci+1,lba-1,cba-1,m_dup)

                DO CASE
                        CASE LASTKEY()=27
                                EXIT
                        CASE LASTKEY() = 13
                                GO m_pos[point]
*                               ver_ven(cod_vend)
                                IF ! BLOQREG()
                                        atencao('Nao foi possivel acessar o registro..')
                                        LOOP
                                ENDIF
                                tela1 := SAVESCREEN(00,00,24,79)
                                botao(09,06,19,60)
                                DEVPOS(10,08);DEVOUT('No.da Duplicata ....:')
                                setcor(3)
                                DEVPOS(10,COL()+1);DEVOUT(dupp->duplicata)
                                setcor(1)
                                DEVPOS(11,08);DEVOUT('Vlr. Duplicata ...R$:')
                                setcor(3)
                                DEVPOS(11,COL()+1);DEVOUT(TRANSFORM(dupp->valor,'9,999,999.99'))
                                setcor(1)
                                DEVPOS(12,08);DEVOUT('Juros Pago .......R$:')
                                setcor(3)
                                DEVPOS(12,COL()+1);DEVOUT(TRANSFORM(dupp->juros,'9,999,999.99'))
                                setcor(1)
                                DEVPOS(13,08);DEVOUT('Desconto .........R$:')
                                setcor(3)
                                DEVPOS(13,COL()+1);DEVOUT(TRANSFORM(dupp->desc,'9,999,999.99'))
                                setcor(1)
                                DEVPOS(14,08);DEVOUT('Valor Pago .......R$:')
                                setcor(3)
                                DEVPOS(14,COL()+1);DEVOUT(TRANSFORM(dupp->vlpago,'9,999,999.99'))
                                setcor(1)
                                DEVPOS(15,08);DEVOUT('Data Vencimento...R$:')
                                setcor(3)
                                DEVPOS(15,COL()+1);DEVOUT(DTOC(dupp->venc))
                                setcor(1)
                                DEVPOS(16,08);DEVOUT('Data Pagamento ...R$:')
                                setcor(3)
                                DEVPOS(16,COL()+1);DEVOUT(DTOC(dupp->datpag))
                                setcor(1)
                                DEVPOS(17,08);DEVOUT('Pago efetuado Com...:')
                                setcor(3)
                                DEVPOS(17,COL()+1);DEVOUT(STRZERO(dupp->datpag-dupp->venc,3)+' Dias.')
                                setcor(1)
                                DEVPOS(18,COL()+1);DEVOUT('Cheque:')
                                setcor(3)
                                DEVPOS(18,COL()+1);DEVOUT(cheque)
                                mensagem('Confirma o estorno do Pagamento da Duplicata [S/n]:')
                                opcao := 'S'
                                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        DBUNLOCK()
                                        RESTSCREEN(00,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                dupp->datpag := CTOD('  /  /  ')
                                dupp->vlpago := 0
                                dupp->pago := ' '
                                dupp->cheque := SPACE(8)
                                dupp->operador := cod_operado
                                DBUNLOCK()
                                DBCOMMIT()
                                RESTSCREEN(00,00,24,79,tela1)
                                LOOP
                ENDCASE
        ENDDO
ENDDO
******************************* FIM *****************************************

