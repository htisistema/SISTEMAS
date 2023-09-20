***
*** CON253.PRG: TRANFERENCIA de Duplicatas (Receber)
***
*********************************************

FUNCTION con253()
***************
LOCAL MPRG:='CON253'
LOCAL tela,lin,mtipo,mduplicata,mnumero,mvendedor,mtip_cli,mcliente,memissao,;
      mvenc,mbanco,magencia,mc_c,mvalor,mnum_ped,mcomissao,m_dupr:={},m_pos:={},;
      mat_dup:={},i

LOCAL li,ci,la,ca,tela1,point,opcao,lc,cc,lb,cb,mcod_cli,;
      mtot_valor,mpos_ini

IF ! ver_nivel(mprg,'CONTA A RECEBER (TRANSFERENCIA DE DOCUMENTO)','15',nivel_acess)
        RETURN NIL
ENDIF

lc := 03
cc := 02
lb := 22
cb := 78

li := 08
ci := 33
la := 22
ca := 79

tela := SAVESCREEN(01,00,24,79)

lin=1
mcod_cli := 0
mforn_aux := 0
mduplicata := SPACE(10)
*------------------------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacfin','fin');RETURN NIL;ENDIF
IF ! AbriArq('saccaixa','caix');RETURN NIL;ENDIF
IF ! AbriArq('saccarta','car');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccheq','cheq');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
IF ! AbriArq('sacped_s','ped_s');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
SET KEY -6 TO f7_cli()
WHILE .T.
        ASIZE(mat_dup,0)
        *****************
        SELE('dupr');ORDSETFOCUS(1)
        *****************
        botao(lc,cc,lb,cb)
        @ lc+9,cc+1 TO lc+9,cb-1
        janela(lc,cc," Contas a Receber ",'*','*')
        janela(lc,cb," TRANSFERENCA DOCUMENTOS ")
        DEVPOS(lc+1,cc+1);DEVOUT('Tipo do Documento........:')
        DEVPOS(lc+2,cc+1);DEVOUT('Numero...................:')
        DEVPOS(lc+3,cc+1);DEVOUT('Numero Documento.........:')
        DEVPOS(lc+4,cc+1);DEVOUT('Codigo Cliente...........:')
        DEVPOS(lc+5,cc+1);DEVOUT('Data Emissao.............:')
        DEVPOS(lc+6,cc+1);DEVOUT('Data Vencimento..........:')
        DEVPOS(lc+7,cc+1);DEVOUT('Valor Documento..........:')
        DEVPOS(lc+8,cc+1);DEVOUT('Numero Pedido............:')

        DEVPOS(lc+10,cc+1);DEVOUT('Tipo Documentos........:')
        DEVPOS(lc+11,cc+1);DEVOUT('No.Banco/Cod.Cartao....:')
        DEVPOS(lc+11,cc+31);DEVOUT('Agencia:')
        DEVPOS(lc+11,cc+50);DEVOUT('C/C:')
        DEVPOS(lc+12,cc+1);DEVOUT('Numero Documentos......:')
        DEVPOS(lc+13,cc+1);DEVOUT('Data de Emissao........:')
        DEVPOS(lc+13,cc+39);DEVOUT('Vencimento:')
        DEVPOS(lc+14,cc+1);DEVOUT('Valor Documento R$.....:')
        DEVPOS(lc+14,cc+41);DEVOUT('Pag.[C]arteria [B]anco:')
        DEVPOS(lc+15,cc+1);DEVOUT('Nome do Correntista....:')
        DEVPOS(lc+16,cc+1);DEVOUT('OBS.:')
        @ lc+17,cc+1 TO lc+17,cb-1
        DEVPOS(lc+18,cc+1);DEVOUT('Valor Processado:')
        Mensagem('Digite o Tipo e Numero do Documento. [ESC] Abandona.')
        mcliente := SPACE(40)
        mtipo := SPACE(2)
        mduplicata=SPACE(10)
        mnumero := SPACE(3)
        @ lc+1,cc+28 GET mtipo PICT '@!' VALID mtipo $ 'DU,CH,CT,FI,RE,  '
        @ lc+2,cc+28 GET mnumero PICT '@!' WHEN mtipo <> 'DU' .AND. ! EMPTY(mtipo)
        @ lc+3,cc+28 GET mduplicata PICT '@!' WHEN ! EMPTY(mtipo)
        READ
        IF LASTKEY()=27         // .OR. mduplicata=SPACE(10)
                RESTSCREEN(01,00,24,79,tela)
                SET KEY -6 TO
                RETURN
        ENDIF
        IF EMPTY(mtipo) .AND. EMPTY(mnumero) .OR. EMPTY(mduplicata)
                @ lc+4,cc+28 GET mcod_cli PICT '99999' VALID ver_cli(mcod_cli,lc+4,cc+35)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mcod_cli := VAL(cli->cod_cli)
                tela1 := SAVESCREEN(01,00,24,79)
                mtotal_doc := 0
                mcont_doc := 0
                ASIZE(m_dupr,0)
                ASIZE(m_pos,0)
                SELE('dupr');ORDSETFOCUS(2)
                GO TOP
                IF dupr->(DBSEEK(STRZERO(mcod_cli,5)))
                        setcor(3)
                        DEVPOS(lc+4,cc+35);DEVOUT(dupr->cliente)
                        setcor(1)
                        WHILE dupr->fornec = STRZERO(mcod_cli,5) .AND. ! EOF()
                                IF ! EMPTY(dupr->datpag)
                                        SKIP
                                        LOOP
                                ENDIF
                                AADD(m_dupr,' '+dupr->tipo+' '+dupr->duplicata+'  '+dupr->numero+'   '+DTOC(dupr->venc)+'   '+TRANSFORM(dupr->valor,'999,999.99')+' '+dupr->conta)
                                AADD(m_pos,RECNO())
                                mcont_doc ++
                                mtotal_doc := mtotal_doc + dupr->valor
                                SKIP
                        ENDDO
                        IF LEN(m_dupr) = 0
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
                        point := ACHOICE(li+3,ci+1,la-3,ca-1,m_dupr,,,point)

                        DO CASE
                                CASE LASTKEY()=27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                CASE LASTKEY() = 13
                                        RESTSCREEN(01,00,24,79,tela1)
                                        GO m_pos[point]
                                        mtipo      := dupr->tipo
                                        mnumero    := dupr->numero
                                        mduplicata := dupr->duplicata
                                        mtip_cli   := dupr->tip_cli
                                        mcliente   := dupr->cliente
                                        memissao   := dupr->emissao
                                        mvenc      := dupr->venc
                                        mbanco     := dupr->banco
                                        magencia   := dupr->agencia
                                        mc_c       := dupr->c_c
                                        mvalor     := dupr->valor
                                        mvendedor  := dupr->vendedor
                                        mcomissao  := dupr->comissao
                                        mnum_ped   := dupr->num_ped
                                        setcor(3)
                                        @ lc+1,cc+28 SAY dupr->tipo
                                        @ lc+2,cc+28 SAY dupr->numero
                                        @ lc+3,cc+28 SAY dupr->duplicata+conta
                                        @ lc+4,cc+35 SAY dupr->cliente
                                        @ lc+5,cc+28 SAY dupr->emissao
                                        @ lc+6,cc+28 SAY dupr->venc
                                        @ lc+7,cc+28 SAY dupr->valor PICT "99,999,999.99"
                                        @ lc+8,cc+28 SAY dupr->num_ped
                                        setcor(1)
                                        mensagem('Confirma a operacao [S/n]:')
                                        opcao := 'S'
                                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                        READ
                                        IF LASTKEY()=27 .OR. opcao = 'N'
                                                LOOP
                                        ENDIF

                        ENDCASE
                ELSE
                        atencao('Nao existe nenhum Documento a ser baixado')
                        LOOP
                ENDIF
        ELSE
                SEEK mtipo+mnumero+mduplicata+' '
                IF ! FOUND()
                        atencao('Documento nao Cadastrado ou Documento PAGO !!!')
                        LOOP
                ENDIF
                RESTSCREEN(01,00,24,79,tela1)
                mtipo      := dupr->tipo
                mnumero    := dupr->numero
                mduplicata := dupr->duplicata
                mtip_cli   := dupr->tip_cli
                mcod_cli   := VAL(dupr->fornec)
                mcliente   := dupr->cliente
                memissao   := dupr->emissao
                mvenc      := dupr->venc
                mbanco     := dupr->banco
                magencia   := dupr->agencia
                mc_c       := dupr->c_c
                mvalor     := dupr->valor
                mvendedor  := dupr->vendedor
                mcomissao  := dupr->comissao
                mnum_ped   := dupr->num_ped
                setcor(3)
                @ lc+1,cc+28 SAY dupr->tipo
                @ lc+2,cc+28 SAY dupr->numero
                @ lc+3,cc+28 SAY dupr->duplicata+conta
                @ lc+4,cc+35 SAY dupr->cliente
                @ lc+5,cc+28 SAY dupr->emissao
                @ lc+6,cc+28 SAY dupr->venc
                @ lc+7,cc+28 SAY dupr->valor PICT "99,999,999.99"
                @ lc+8,cc+28 SAY dupr->num_ped
                setcor(1)
                mensagem('Confirma a operacao [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY()=27 .OR. opcao = 'N'
                        LOOP
                ENDIF
        ENDIF
        mpos_ini := RECNO()
        mtot_valor := 0
        WHILE mvalor <> mtot_valor
                setcor(3)
                DEVPOS(lc+18,cc+19);DEVOUT(TRANSFORM(mtot_valor,'9,999,999.99'))
                setcor(1)
                IF mtot_valor > mvalor
                        atencao('Valor Digitado estar MAIOR que o valor da Duplicata Inicial - Digite tudo novamente')
                        mtot_valor := 0
                        ASIZE(mat_dup,0)
                ENDIF
                mTtipo=SPACE(2)
                mTnum_banco := 0
                mTagencia := SPACE(8)
                mTc_c := SPACE(13)
                mTduplicata=SPACE(10)
                mTemissao=mdata_sis
                mTvenc=CTOD("  /  /  ")
                mTvalor=0
                mTbanco := 'C'
                mTcorrente := SPACE(35)
                mTobs := 'Transf.: Tipo:'+mtipo+' - Documento:'+mduplicata

                mdesc_cart := 0
                mprazo_cart:= 0
                mcartao := SPACE(20)
                Mensagem('Digite o Numero da Duplicata. [ESC] Abandona.')
                @ lc+10,cc+27 GET mTtipo PICT '@!' VALID IF(mTtipo='CT',lim_get() .AND. limpa(lc+3,cc+32,lc+3,cb-1),mTtipo $ 'CH,DU,CT,FI,RE' .AND. lim_get()) WHEN men_get(lc+11,cc+25,'Informe o tipo de documento <DU>duplicata <CH>cheques <CT>cartao <FI>financiamento e <RE>recebimento')
                @ lc+11,cc+27 GET mTnum_banco PICT '999' VALID IF(mTtipo='CT',ver_cartao(mTnum_banco,lc+12,cc+31),.T.) .AND. lim_get() WHEN mTtipo <> 'DU' .AND. men_get(lc+11,cc+25,'Informe o No.do banco ou se tipo de documento for "CT" informe o codigo do cartao')
                @ lc+11,cc+41 GET mTagencia PICT '@!' VALID lim_get() WHEN mTtipo = 'CH' .AND. men_get(lc+12,cc+41,'Informe a AGENCIA BANCARIA')
                @ lc+11,cc+56 GET mTc_c PICT '@!' VALID lim_get()  WHEN mTtipo = 'CH' .AND. men_get(lc+12,cc+56,'Informe a C/C BANCARIA')
                @ lc+12,cc+27 GET mTduplicata PICT '@!' VALID lim_get() WHEN men_get(lc+13,cc+25,'Informe o No.do documento')
                READ
                IF LASTKEY()=27
                        EXIT
                ENDIF
                Mensagem('Complete os Dados. [ESC] Abandona.')
                *****************
                SELE('dupr');ORDSETFOCUS(1)
                GO TOP
                *****************
                IF mTtipo = 'CH'
                        qtd_chq(mcod_cli,STRZERO(mTnum_banco,3),mTagencia,mTc_c)
                ENDIF
                IF mTnum_banco = 0
                        SEEK mTtipo+SPACE(3)+mTduplicata+' '+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
                ELSE
                        SEEK mTtipo+STRZERO(mTnum_banco,3)+mTduplicata+' '+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
                ENDIF
                IF FOUND()
                        IF mTtipo = 'CH'
                                atencao('Este Cheque ja existe esta em ABERTO !')
                        ELSEIF mTtipo = 'DU'
                                atencao('Esta Duplicata ja existe esta em ABERTO !')
                        ELSEIF mTtipo = 'FI'
                                atencao('Esta fianaciamento ja existe esta em ABERTO!')
                        ELSE
                                atencao('Este Cartao ja existe esta em ABERTO !')
                        ENDIF
                        Mensagem('Deseja Incluir mesmo assim ? [S/n]:')
                        opcao := 'N'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = "N"
                                LOOP
                        ENDIF
                ENDIF
                IF mTnum_banco = 0
                        SEEK mTtipo+SPACE(3)+mTduplicata+'B'+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
                ELSE
                        SEEK mTtipo+STRZERO(mTnum_banco,3)+mTduplicata+'B'+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
                ENDIF
                IF FOUND()
                        IF mTtipo = 'CH'
                                atencao('Este Cheque ja existe e ja foi PAGO !')
                        ELSEIF mTtipo = 'DU'
                                atencao('Esta Duplicata ja existe e ja foi PAGA !')
                        ELSEIF mTtipo = 'FI'
                                atencao('Esta Financiamento ja existe e ja foi PAGO !')
                        ELSE
                                atencao('Este Cartao ja existe e ja foi PAGO !')
                        ENDIF
                        Mensagem('Deseja Incluir mesmo assim ? [S/n]:')
                        opcao := 'N'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = "N"
                                LOOP
                        ENDIF
                ENDIF
                IF LASTKEY()=27
                        RETURN NIL
                ENDIF
                mTvenc := mvenc
                mTemissao := memissao
                mTvalor := mvalor - mtot_valor
                mTbanco := SPACE(1)
                @ lc+13,cc+27 GET mTemissao
                @ lc+13,cc+52 GET mTvenc VALID vencim(mTvenc,,,mTemissao)
                @ lc+14,cc+27 GET mTvalor PICT "9,999,999.99" VALID mTvalor>0 .AND. spc(mcod_cli,mTvalor,,)
                @ lc+14,cc+65 GET mTbanco PICT "@!" VALID mTbanco $ 'C,B' .AND. lim_get() WHEN men_get(lc+15,cc+23,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO')
                @ lc+15,cc+27 GET mTcorrente PICT "@!" VALID lim_get() WHEN men_get(lc+16,cc+23,'Informe o nome do correntista')
                @ lc+16,cc+07 GET mTobs
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                Mensagem('Confirma os Dados ? [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = "N"
                        LOOP
                ENDIF
                AADD(mat_dup,{mTtipo,mTnum_banco,mTagencia,mTc_c,mTduplicata,;
                              mTemissao,mTvenc,mTvalor,mTbanco,mTcorrente,;
                              mTobs})
                mtot_valor := mtot_valor + mTvalor
        ENDDO
        IF mvalor > mtot_valor
                atencao('Valor Digitado menor que o Documento')
        ENDIF
        Mensagem('Confirma o processamento dos Dados ? [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao estar sendo possivel bloquear o registro')
                LOOP
        ENDIF
        mTobs := 'Transf.: '
        i := 0
        FOR i = 1 TO LEN(mat_dup)
                mTobs := mTobs + mat_dup[i,1]+mat_dup[i,4]+'-'
        NEXT
        GO mpos_ini
        BLOQREG()
        dupr->datpag := mdata_sis
        dupr->vlpago := dupr->valor
        dupr->pago := "T"
        dupr->obs := mTobs
        DBCOMMIT()
        DBUNLOCK()
        i := 0
        FOR i = 1 TO LEN(mat_dup)
                IF ! ADIREG()
                        atencao('Nao foi possivel adicionar o registro')
                        LOOP
                ENDIF
                dupr-> tipo := mat_dup[i,1]
                IF EMPTY(mat_dup[i,2])
                        dupr-> numero := SPACE(3)
                ELSE
                        dupr-> numero := STRZERO(mat_dup[i,2],3)
                ENDIF
                dupr-> agencia := mat_dup[i,3]
                dupr-> c_c := mat_dup[i,4]
                dupr-> duplicata := mat_dup[i,5]
                dupr-> emissao := mat_dup[i,6]
                dupr-> venc := mat_dup[i,7]
                dupr-> valor_dup := mat_dup[i,8]
                dupr-> tip_cli := mtip_cli
                dupr-> fornec := STRZERO(mcod_cli,5)
                dupr-> cliente := mcliente
                IF mat_dup[i,1] = 'CT'
                        dupr-> valor := mat_dup[i,8] - (mat_dup[i,8] * (mdesc_cart/100))
                ELSE
                        dupr-> valor := mat_dup[i,8]
                ENDIF
                dupr-> pago := " "
                dupr-> vendedor := mvendedor
                dupr-> num_ped := mnum_ped
                dupr-> banco := mat_dup[i,9]
                dupr-> comissao := mcomissao
                dupr-> corrente := mat_dup[i,10]
                dupr-> obs      := mat_dup[i,11]
                IF mat_dup[i,6] = mat_dup[i,7]
                        mensagem('Deseja fazer a BAIXA deste documento agora [S/n]:')
                        opcao := 'S'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        IF opcao = 'S'
                                dupr->datpag := mat_dup[i,7]
                                dupr->vlpago := mat_dup[i,8]
                                dupr->pago := "B"
                                dupr->banco := 'C'
                        ENDIF
                ENDIF
                dupr-> operador := cod_operado
                DBCOMMIT()
                DBUNLOCK()
        NEXT
        IF mvalor > mtot_valor
                IF ! ADIREG()
                        atencao('Nao foi possivel adicionar o registro')
                        LOOP
                ENDIF
                dupr-> tipo := mtipo
                dupr-> numero := mnumero
                dupr-> agencia := magencia
                dupr-> c_c := mc_c
                dupr-> duplicata := mduplicata
                dupr-> emissao := memissao
                dupr-> venc := mvenc
                dupr-> valor_dup := mvalor
                dupr-> tip_cli := mtip_cli
                dupr-> fornec := STRZERO(mcod_cli,5)
                dupr-> cliente := mcliente
                dupr-> valor := mvalor - mtot_valor
                dupr-> pago := " "
                dupr-> vendedor := mvendedor
                dupr-> num_ped := mnum_ped
                dupr-> banco := mbanco
                dupr-> comissao := mcomissao
                dupr-> obs      := mTobs
                dupr-> operador := cod_operado
                dupr-> conta := 'T'
                DBCOMMIT()
                DBUNLOCK()
        ENDIF
ENDDO

