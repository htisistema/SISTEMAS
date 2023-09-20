***
*** CON251.PRG: Baixa INDIVIDUAL de Duplicatas (APAGAR)
***
*********************************************

FUNCTION con22()
***************
LOCAL MPRG:='CON22'
LOCAL tela,lin,mtipo,mduplicata,mcheque,mdatpag,mvlpago,mnota,mtipo_comp,;
      mcod_vend,mforn_aux,opcao,mbanco,lc,cc,lb,cb,mcomissao,mnumero,;
      pconta
LOCAL mvalor_dup,mcliente,memissao,mvenc,mnum_ped,mvalor_cont,mcont_doc,;
      mtotal_doc,m_dupp:={},m_pos:={},li,ci,la,ca,tela1,point

PRIVATE mcod_forn

IF ! ver_nivel(mprg,'BAIXA INDIVIDUAL DE DOCUMENTOS (C.APAGAR)','15')
        RETURN NIL
ENDIF

lc := 07
cc := 02
lb := 17
cb := 78

li := 08
ci := 33
la := 22
ca := 79

tela := SAVESCREEN(01,00,24,79)

lin=1
mcod_forn:= 0
mforn_aux := 0
mduplicata := SPACE(10)
*------------------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
SET KEY -5 TO f6_ccpg()
WHILE .T.
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        *****************
        botao(lc,cc,lb,cb)
        janela(lc,cc," Contas a PAGAR ",'*','*')
        janela(lc,cb," Baixa de Documentos ")
        DEVPOS(lc+1,cc+1);DEVOUT('Tipo do Documento........:')
        DEVPOS(lc+2,cc+1);DEVOUT('Numero...................:')
        DEVPOS(lc+3,cc+1);DEVOUT('Numero Documento.........:')
        DEVPOS(lc+4,cc+1);DEVOUT('Codigo Fornecedor/Outros.:')
        DEVPOS(lc+5,cc+1);DEVOUT('Data Emissao.............:')
        DEVPOS(lc+6,cc+1);DEVOUT('Data Vencimento..........:')
        DEVPOS(lc+7,cc+1);DEVOUT('Data Pagamento...........:')
        DEVPOS(lc+8,cc+1);DEVOUT('Valor a Pagar............:')
        DEVPOS(lc+9,cc+1);DEVOUT('Numero Cheque............:')
        Mensagem('Digite o Tipo e Numero do Documento. [ESC] Abandona.')
        mvalor_dup :=0
        mnota := SPACE(8)
        mcliente := SPACE(40)
        mtipo_comp := SPACE(2)
        mcod_vend := SPACE(3)
        mtipo := SPACE(2)
        mduplicata=SPACE(10)
        mcheque=SPACE(8)
        mbanco := SPACE(1)
        mcomissao := 0
        mnumero := SPACE(3)
        pconta := ' '
        mdias_atras := 0
        @ lc+1,cc+28 GET mtipo PICT '@!' VALID mtipo $ 'DU,CH,PA,FR,  '
        @ lc+2,cc+28 GET mnumero PICT '@!' WHEN mtipo <> 'DU' .AND. ! EMPTY(mtipo)
        @ lc+3,cc+28 GET mduplicata PICT '@!' WHEN ! EMPTY(mtipo)
        READ
        IF LASTKEY()=27         // .OR. mduplicata=SPACE(10)
                RESTSCREEN(01,00,24,79,tela)
                SET KEY -5 TO
                RETURN
        ENDIF
        IF EMPTY(mtipo) .AND. EMPTY(mnumero) .OR. EMPTY(mduplicata)
                @ lc+4,cc+28 GET mcod_forn PICT '99999' VALID v_fornece(mcod_forn,lc+4,cc+35)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mcod_forn := VAL(ccpg->cod_forn)
                tela1 := SAVESCREEN(01,00,24,79)
                mtotal_doc := 0
                mcont_doc := 0
                ASIZE(m_dupp,0)
                ASIZE(m_pos,0)
                SELE('dupp');ORDSETFOCUS(2)
                GO TOP
                IF dupp->(DBSEEK(STRZERO(mcod_forn,4)))
                        setcor(3)
                        DEVPOS(lc+4,cc+35);DEVOUT(dupp->cliente)
                        setcor(1)
                        WHILE dupp->fornec = STRZERO(mcod_forn,4) .AND. ! EOF()
                                IF ! EMPTY(dupp->datpag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_dupp,' '+dupp->tipo+' '+dupp->duplicata+'  '+dupp->numero+'   '+DTOC(dupp->venc)+'   '+TRANSFORM(dupp->valor,'999,999.99')+' '+dupp->conta)
                                AADD(m_pos,RECNO())
                                mcont_doc ++
                                mtotal_doc := mtotal_doc + dupp->valor
                                SKIP
                        ENDDO
                        IF LEN(m_dupp) = 0
                                atencao('Nao existe nenhum Documento a ser baixado')
                                LOOP
                        ENDIF
                        mensagem('<> Consulta  - <ESC> Retorna  -  <ENTER> p/Confirmar')
                        botao(li,ci,la,ca)
                        setcor(3)
                        DEVPOS(li+1,ci+01);DEVOUT('Documento')
                        DEVPOS(li+1,ci+16);DEVOUT(' No.')
                        DEVPOS(li+1,ci+22);DEVOUT('Vencimento')
                        DEVPOS(li+1,ci+39);DEVOUT('Valor')
                        @ li+2,ci+1 TO li+2,ca-1
                        @ la-2,ci+1 TO la-2,ca-1
                        DEVPOS(la-1,ci+1);DEVOUT('Quantidade:')
                        DEVPOS(la-1,ci+24);DEVOUT('Total:')
                        setcor(1)
                        DEVPOS(la-1,ci+13);DEVOUT(STRZERO(mcont_doc,4))
                        DEVPOS(la-1,ci+31);DEVOUT(TRANSFORM(mtotal_doc,'9,999,999.99'))
                        point := ACHOICE(li+3,ci+1,la-3,ca-1,m_dupp,,,point)

                        DO CASE
                                CASE LASTKEY()=27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                CASE LASTKEY() = 13
                                        RESTSCREEN(01,00,24,79,tela1)
                                        GO m_pos[point]
                                        mjuros := 0
                                        mmulta := 0
                                        mdatpag=mdata_sis
                                        mvlpago=dupp->valor
                                        mbanco := SUBSTR(banco,1,1)
                                        mvlpago=dupp->valor
                                        mduplicata := dupp->duplicata
                                        mtipo := dupp->tipo
                                        setcor(3)
                                        @ lc+1,cc+28 SAY dupp->tipo
                                        @ lc+2,cc+28 SAY dupp->numero
                                        @ lc+3,cc+28 SAY dupp->duplicata+conta
                                        @ lc+4,cc+35 SAY dupp->cliente
                                        @ lc+5,cc+28 SAY dupp->emissao
                                        @ lc+6,cc+28 SAY dupp->venc
                                        @ lc+8,cc+28 SAY dupp->valor PICT "99,999,999.99"
                                        setcor(1)
                                        @ lc+7,cc+28 GET mdatpag
                                        READ
                                        IF LASTKEY()=27
                                                LOOP
                                        ENDIF
                                        mvlpago=dupp->valor
                                        @ lc+8,cc+28 GET mvlpago PICT "99,999,999.99"
                                        @ lc+9,cc+28 GET mcheque PICT '99999999'
                                        READ
                                        IF LASTKEY()=27
                                                LOOP
                                        ENDIF
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhum Documento a ser baixado')
                        LOOP
                ENDIF
        ELSE
                IF ! dupp->(DBSEEK(mtipo+mduplicata))
                        atencao('Documento nao Cadastrado ou Documento PAGO !!!')
                        LOOP
                ENDIF
                IF FOUND() .AND. pago = 'B'
                        WHILE ! EOF()
                                IF mtipo = tipo .AND. mduplicata = duplicata .AND. pago = ' '
                                        EXIT
                                ENDIF
                                SKIP
                        ENDDO
                        IF mtipo <> tipo .OR. mduplicata <> duplicata .OR. pago = 'B'
                                atencao('Documento nao Cadastrado ou Documento PAGO !!!')
                                LOOP
                        ENDIF
                ENDIF
                mcod_forn := VAL(dupp->fornec)
                mforn_aux := VAL(dupp->fornec)
                Mensagem('Digite o Codigo do cliente ou [F6]p/consultar. [ESC] Abandona.')
                @ lc+4,cc+28 GET mcod_forn PICT '99999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF mcod_forn <> mforn_aux
                        GO TOP
                        SEEK mtipo+mnumero+mduplicata+' '+STRZERO(mcod_forn,4)
                        IF ! FOUND()
                                atencao('Nao existe esta duplicata ou ja foi PAGA !!!')
                                LOOP
                        ENDIF
                ENDIF
                mdatpag=mdata_sis
                mvlpago=dupp->valor
                mbanco := SUBSTR(banco,1,1)
                mvlpago=dupp->valor
                setcor(3)
                @ lc+3,cc+28 SAY mduplicata+conta
                @ lc+4,cc+35 SAY dupp->cliente
                @ lc+5,cc+28 SAY dupp->emissao
                @ lc+6,cc+28 SAY dupp->venc
                @ lc+8,cc+28 SAY dupp->valor PICT "99,999,999.99"
                setcor(1)
                @ lc+7,cc+28 GET mdatpag
                READ
                IF LASTKEY()=27
                        LOOP
                ENDIF
                mvlpago=dupp->valor
                @ lc+8,cc+28 GET mvlpago PICT "99,999,999.99"
                @ lc+9,cc+28 GET mcheque PICT '99999999'
                READ
                IF LASTKEY()=27
                        LOOP
                ENDIF
        ENDIF
        Mensagem('Confirma os Dados ? [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF mvlpago < valor
                mensagem('Este Valor vai ser por CONTA [S/n]:')
                pconta := 'S'
                @ MAXROW()-1,COL()+1 GET pconta PICT '@!' VALID pconta $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
        IF BLOQREG()
                dupp->datpag := mdatpag
                dupp->vlpago := mvlpago
                dupp->cheque := mcheque
                IF pconta = 'S'
                        pconta := '*'
                        mcliente := cliente
                        mvalor_dup := valor_dup
                        mvalor_cont := valor-mvlpago
                        memissao := emissao
                        mvenc := venc
                        mcod_vend := vendedor
                        mnum_ped := num_ped
                        dupp->valor := mvlpago
                        dupp->conta := pconta
                ELSE
                        IF mvlpago<valor
                                dupp->desc := valor-mvlpago
                        ELSE
                                dupp->juros := mvlpago-valor
                        ENDIF
                ENDIF
                dupp->pago := "B"
                dupp->banco := mbanco
                dupp->operador := cod_operado
                COMMIT
                UNLOCK
        ELSE
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
        IF pconta = '*'
                IF ADIREG()
                        dupp-> tipo := mtipo
                        dupp-> numero := mcheque
                        dupp-> duplicata := mduplicata
                        dupp-> valor_dup := mvalor_dup
                        dupp-> fornec := STRZERO(mfornec,4)
                        dupp-> cliente := mcliente
                        dupp-> emissao := memissao
                        dupp-> venc := mvenc
                        dupp-> valor := mvalor_cont
                        dupp-> pago := " "
                        dupp-> vendedor := mcod_vend
                        dupp-> num_ped := mnum_ped
                        dupp-> conta := pconta
                        dupp-> banco := mbanco
                        dupp-> operador := cod_operado
                ENDIF
        ENDIF
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        *****************
ENDDO

