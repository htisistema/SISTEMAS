***
*** CON251.PRG: Baixa INDIVIDUAL de Duplicatas (Receber)
***             ALTERACAO E ESTORNO
*********************************************

FUNCTION con251(cx,mtp)
***************
LOCAL MPRG:='CON251'

LOCAL tela,lin,mcheque,mdatpag,mvlpago,mnota,mtipo_comp,;
      mcod_vend,mforn_aux,opcao,mbanco,lc,cc,lb,cb,mcomissao,mnumero,;
      pconta,mtip_cli,mvalor_dup,mcliente,memissao,mvenc,mnum_ped,mvalor_cont,;
      mdias_atras,mjuros,mmulta,mcont_doc,mtotal_doc,m_dupr:={},m_pos:={},m_dup:={},m_po:={}
LOCAL li,ci,la,ca,tela1,point,mqtd_doc,mqtd_dias,mobs,mtraco:=REPLI('=',80)

LOCAL lci,cci,lba,cba,mtel,mnome_cli := SPACE(40),mtip_pg:='  ',mtela_aut,mtipo_aut

PRIVATE mcod_cli,mtipo,mmduplicata

IF mtp = NIL
        IF cx <> NIL
                IF ! ver_nivel(mprg+'CX','CONTA A RECEBAR (BAIXA INDIVIDUAL DOCUMENTOS CAIXA)','135',nivel_acess)
                        RETURN NIL
                ENDIF
        ELSE
                IF ! ver_nivel(mprg+'MV','CONTA A RECEBER (BAIXA INDIVIDUAL DOCUMENTOS MOVIMENTO)','135',nivel_acess)
                        RETURN NIL
                ENDIF
        ENDIF
ELSEIF mtp = 'S'
        IF ! ver_nivel(mprg+'ES','CONTA A RECEBER (ESTORNO DE DOCUMENTOS RECEBIDO)','135',nivel_acess)
                RETURN NIL
        ENDIF
ELSEIF mtp = 'A'
        IF ! ver_nivel(mprg+'AL','CONTA A RECEBER (ALTERACAO DE DOCUMENTOS A RECEBER)','135',nivel_acess)
                RETURN NIL
        ENDIF
ENDIF
lc := 05
cc := 02
lb := 21
cb := 78

li := 07
ci := 29
la := 22
ca := 79

tela := SAVESCREEN(01,00,24,79)

lin=1
mcod_cli := 0
mforn_aux := 0
mduplicata := SPACE(10)
*------------------------------------------------------------------------------------
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccheq','cheq');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
IF ! AbriArq('saccaixa','caix');RETURN NIL;ENDIF
IF ! AbriArq('sacban1','ban1');RETURN NIL;ENDIF
IF ! AbriArq('sacban2','ban2');RETURN NIL;ENDIF
IF ! AbriArq('sacban3','ban3');RETURN NIL;ENDIF
IF ! AbriArq('sacban4','ban4');RETURN NIL;ENDIF
IF ! AbriArq('sacban5','ban5');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
SET KEY -6 TO f7_cli()
SET KEY 275 TO cons_doc()
WHILE .T.
        IF ver_serie() = '141278' .OR. ver_serie() = '141277'
                mcod_cli := 0
        ENDIF
        *****************
        SELE('dupr');ORDSETFOCUS(1)
        *****************
        botao(lc,cc,lb,cb)
        janela(lc,cc," Contas a Receber ",'*','*')
        IF mtp = 'S'
                janela(lc,cb," Estorno de Documentos INDIVIDUAL ")
        ELSEIF mtp = NIL
                janela(lc,cb," Baixa de Documentos INDIVIDUAL ")
        ELSEIF mtp = 'A'
                janela(lc,cb," Alteracao de Documentos a Receber ")
        ENDIF
        DEVPOS(lc+1,cc+1);DEVOUT('Codigo Cliente...........:')
        DEVPOS(lc+2,cc+1);DEVOUT('Tipo do Documento........:')
        DEVPOS(lc+3,cc+1);DEVOUT('Numero...................:')
        DEVPOS(lc+4,cc+1);DEVOUT('Numero Documento.........:')
        DEVPOS(lc+5,cc+1);DEVOUT('Data Emissao.............:')
        DEVPOS(lc+6,cc+1);DEVOUT('Data Vencimento..........:')
        DEVPOS(lc+7,cc+1);DEVOUT('Dias em Atraso...........:')
        DEVPOS(lc+7,cc+1);DEVOUT('Dias em Atraso...........:')
        DEVPOS(lc+8,cc+1);DEVOUT('Data Pagamento...........:')
        DEVPOS(lc+9,cc+1);DEVOUT('Valor Documento..........:')
        DEVPOS(lc+10,cc+1);DEVOUT('Multa (%) '+TRANSFORM(set->multa_dup,'99.99')+' .......R$:')
        DEVPOS(lc+11,cc+1);DEVOUT('Juros ao dia (%) '+TRANSFORM(set->juros_dup,'99.99')+' R$:')
        DEVPOS(lc+12,cc+1);DEVOUT('Valor a Pagar............:')
        DEVPOS(lc+13,cc+1);DEVOUT('Tipo de Operacao.........:')
        DEVPOS(lc+14,cc+1);DEVOUT('Numero Cheque............:')
        DEVPOS(lc+15,cc+1);DEVOUT('Obs.:')
        Mensagem('Digite Cod.do Cliente ou tipo e No.Documento - [ESC] Abandona.')
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
        mobs := SPACE(60)
        pconta := ' '
        mdias_atras := 0
        mqtd_dias := 0
        mqtd_doc := 0
        mtip_pg := '  '
        @ lc+1,cc+28 GET mcod_cli PICT '99999' WHEN men_get(lc+2,cc+28,'Informe o Cod.do CLIENTE - [F7]p/Consultar Cliente - [ALT+R]Consulta de Documentos - [ENTER]p/informa o tipo de DOCUMENTO - <ESC>p/abandonar') VALID lim_get() .AND. ver_cli(mcod_cli,lc+1,cc+35)
        @ lc+2,cc+28 GET mtipo PICT '@!' VALID mtipo $ 'DU,CH,CT,FI,RE,  ' WHEN EMPTY(mcod_cli)
        @ lc+3,cc+28 GET mnumero PICT '@!' WHEN mtipo <> 'DU' .AND. ! EMPTY(mtipo) .AND.  EMPTY(mcod_cli)
        @ lc+4,cc+28 GET mduplicata PICT '@!' WHEN ! EMPTY(mtipo) .AND.  EMPTY(mcod_cli)
        READ
        IF LASTKEY()=27         // .OR. mduplicata=SPACE(10)
                RESTSCREEN(01,00,24,79,tela)
                SET KEY -6 TO
                RETURN
        ENDIF
        lim_get()
        IF ! EMPTY(mcod_cli)
*       IF EMPTY(mtipo) .AND. EMPTY(mnumero) .OR. EMPTY(mduplicata)
                ver_cli(mcod_cli,lc+1,cc+35)
                tela1 := SAVESCREEN(01,00,24,79)
                mtotal_doc := 0
                mcont_doc := 0
                ASIZE(m_dupr,0)
                ASIZE(m_pos,0)
                SELE('dupr');ORDSETFOCUS(2)
                GO TOP
                IF dupr->(DBSEEK(STRZERO(mcod_cli,5)))
                        mcliente := dupr->cliente
                        setcor(3)
                        DEVPOS(lc+1,cc+35);DEVOUT(dupr->cliente)
                        setcor(1)
                        WHILE dupr->fornec = STRZERO(mcod_cli,5) .AND. ! EOF()
                                IF mtp = 'S' .AND. EMPTY(dupr->datpag)
                                        SKIP
                                        LOOP
                                ELSEIF mtp = 'A' .OR. mtp = NIL
                                        IF ! EMPTY(dupr->datpag)
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                AADD(m_dupr,' '+dupr->tipo+' '+dupr->duplicata+' '+dupr->tip_pg+'  '+dupr->numero+'   '+DTOC(dupr->venc)+'   '+TRANSFORM(dupr->valor,'999,999.99')+' '+dupr->conta)
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
                        DEVPOS(li+1,ci+16);DEVOUT('Op')
                        DEVPOS(li+1,ci+19);DEVOUT(' No.')
                        DEVPOS(li+1,ci+25);DEVOUT('Vencimento')
                        DEVPOS(li+1,ci+42);DEVOUT('Valor')
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
                                        mjuros := 0
                                        mmulta := 0
                                        mdatpag=mdata_sis
                                        mvlpago=valor
                                        mbanco := SUBSTR(banco,1,1)
                                        mcomissao := comissao
                                        mobs = dupr->obs
                                        mdias_atras := mdatpag-(dupr->venc+set->dia_venc)
                                        IF mdias_atras > 0
                                                mdias_atras := mdatpag-dupr->venc
                                                mmulta := dupr->valor * ((set->multa_dup)/100)
*                                               mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                                mjuros := (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                        ENDIF
                                        mvlpago=valor+mjuros+mmulta
                                        mduplicata := dupr->duplicata
                                        mtipo := dupr->tipo
                                        setcor(3)
                                        @ lc+1,cc+35 SAY dupr->cliente
                                        @ lc+2,cc+28 SAY dupr->tipo
                                        @ lc+3,cc+28 SAY dupr->numero
                                        @ lc+4,cc+28 SAY dupr->duplicata+conta
                                        @ lc+5,cc+28 SAY dupr->emissao
                                        @ lc+6,cc+28 SAY dupr->venc
                                        @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                                        @ lc+9,cc+28 SAY dupr->valor PICT "99,999,999.99"
                                        @ lc+10,cc+28 SAY mmulta PICT "99,999,999.99"
                                        @ lc+11,cc+28 SAY mjuros PICT "99,999,999.99"
                                        setcor(1)
                                        IF mtp = NIL
                                                @ lc+8,cc+28 GET mdatpag
                                                READ
                                                IF LASTKEY()=27
                                                        LOOP
                                                ENDIF
                                                IF mdatpag <> mdata_sis
                                                        IF ! ver_nivel('DOC_DATA','ALTERACAO DATA BAIXA DE DOCUMENTOS A RECEBER','1245',nivel_acess)
                                                                LOOP
                                                        ENDIF
                                                ENDIF
                                                mdias_atras := mdatpag-(dupr->venc+set->dia_venc)
                                                IF mdias_atras > 0
                                                        mdias_atras := mdatpag-dupr->venc
                                                        mmulta := dupr->valor * ((set->multa_dup)/100)
*                                                       mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                                        mjuros := (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                                ELSE
                                                        mjuros := 0
                                                        mmulta := 0
                                                ENDIF
                                                mvlpago=valor+mjuros+mmulta
                                                setcor(3)
                                                @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                                                @ lc+10,cc+28 SAY mmulta PICT "99,999,999.99"
                                                @ lc+11,cc+28 SAY mjuros PICT "99,999,999.99"
                                                setcor(1)
                                                @ lc+12,cc+28 GET mvlpago PICT "99,999,999.99"
                                                @ lc+13,cc+28 GET mtip_pg PICT '@!'
                                                @ lc+14,cc+28 GET mcheque PICT '99999999'
                                                @ lc+15,cc+07 GET mobs
                                                READ
                                                IF LASTKEY()=27
                                                        LOOP
                                                ENDIF
                                        ELSEIF mtp = 'S'
                                                setcor(3)
                                                @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                                                @ lc+8,cc+28 SAY dupr->datpag
                                                @ lc+10,cc+28 SAY "             "
                                                @ lc+11,cc+28 SAY dupr->juros PICT "99,999,999.99"
                                                @ lc+12,cc+28 SAY dupr->vlpago PICT "99,999,999.99"
                                                @ lc+13,cc+28 SAY dupr->tip_pg
                                                @ lc+14,cc+28 SAY dupr->cheque
                                                @ lc+15,cc+07 SAY dupr->obs
                                                setcor(1)
                                        ELSEIF mtp = 'A'
                                                mtel := SAVESCREEN(00,00,24,79)
                                                lci := 03
                                                cci := 00
                                                lba := 19
                                                cba := 79
                                                botao(lci,cci,lba,cba)
                                                @ lci+1,cci+1 TO lci+1,cba-1
                                                DEVPOS(lci,cci+1);DEVOUT(" ALTERACAO DE DOCUMENTO (Contas a Receber) ")
                                                DEVPOS(lci+2,cci+2);DEVOUT('Tipo Documentos........:')
                                                DEVPOS(lci+3,cci+2);DEVOUT('No.Banco/Cod.Cartao....:')
                                                DEVPOS(lci+3,cci+32);DEVOUT('Agencia:')
                                                DEVPOS(lci+3,cci+51);DEVOUT('C/C:')
                                                DEVPOS(lci+4,cci+2);DEVOUT('Numero Documentos......:')
                                                DEVPOS(lci+5,cci+2);DEVOUT('Codigo do Cliente......:')
                                                DEVPOS(lci+6,cci+2);DEVOUT('Data de Emissao........:')
                                                DEVPOS(lci+7,cci+2);DEVOUT('Data de Vencimento.....:')
                                                DEVPOS(lci+8,cci+2);DEVOUT('Valor Documento R$.....:')
                                                DEVPOS(lci+9,cci+2);DEVOUT('Pag. [C]arteria [B]anco:')
                                                DEVPOS(lci+10,cci+2);DEVOUT('Codigo do Vendedor.....:')
                                                DEVPOS(lci+11,cci+2);DEVOUT('Numero do Pedido/NF....:')
                                                DEVPOS(lci+12,cci+2);DEVOUT('% de Comissao .........:')
                                                DEVPOS(lci+13,cci+2);DEVOUT('Nome do Correntista....:')
                                                DEVPOS(lci+14,cci+2);DEVOUT('Tipo de Operacao.......:')
                                                DEVPOS(lci+15,cci+2);DEVOUT('OBS.:')
                                                GO m_pos[point]
                                                mfornec=VAL(dupr->fornec)
                                                mnome_cli := dupr->cliente
                                                memissao=dupr->emissao
                                                mvenc=dupr->venc
                                                mvalor=dupr->valor
                                                mtipo=dupr->tipo
                                                mcod_ven := VAL(dupr->vendedor)
                                                mnum_ped := VAL(dupr->num_ped)
                                                mbanco := SUBSTR(dupr->banco,1,1)
                                                mnum_banco := VAL(dupr->numero)
                                                mcomissao := dupr->comissao
                                                magencia := dupr->agencia
                                                mc_c := dupr->c_c
                                                mcorrente := dupr->corrente
                                                mobs := dupr->obs
                                                mtip_pg := dupr->tip_pg
                                                mensagem('Complete os Dados. [ESC] Abandona.')
                                                IF RTRIM(mnome_cli) = 'DIVERSOS'
                                                        mnome_cli := dupr->cliente
                                                        setcor(3)
                                                        DEVPOS(lci+5,cci+32);DEVOUT(mnome_cli)
                                                        setcor(1)
                                                ENDIF
                                                @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,CT,FI,RE' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <CT>cartao <FI>financiamento <RE>recebimentos')
                                                @ lci+3,cci+27 GET mnum_banco PICT '999' VALID lim_get() WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+31,'Informe o No.do banco')
                                                @ lci+3,cci+41 GET magencia PICT '@!' VALID lim_get() WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+41,'Informe a AGENCIA BANCARIA')
                                                @ lci+3,cci+56 GET mc_c PICT '@!' VALID lim_get() .AND. qtd_chq(mfornec,STRZERO(mnum_banco,3),magencia,mc_c) WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+56,'Informe a C/C BANCARIA')
                                                @ lci+4,cci+27 GET mduplicata PICT '@!' VALID lim_get() WHEN men_get(lci+5,cci+31,'Informe o No.do documento')
                                                @ lci+5,cci+27 GET mfornec PICT "99999" VALID mcli(mfornec,lci+5,cci+27) .AND. lim_get() WHEN men_get(lci+6,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
                                                READ
                                                IF LASTKEY()=27
                                                        RESTSCREEN(00,00,24,79,mtel)
                                                        LOOP
                                                ENDIF
                                                IF RTRIM(mnome_cli) = 'DIVERSOS'
                                                        mnome_cli := dupr->cliente
                                                        @ lci+5,cci+32 GET mnome_cli PICT '@!' VALID lim_get() WHEN men_get(lci+6,cci+23,'Neste campo voce pode descrever o que deseja')
                                                ELSE
                                                        mnome_cli := cli->razao
                                                ENDIF
                                                @ lci+6,cci+27 GET memissao
                                                @ lci+7,cci+27 GET mvenc VALID IF(EMPTY(mvenc),.F.,.T.)
                                                @ lci+8,cci+27 GET mvalor PICT "@E 99,999,999,999.99" VALID mvalor>0    //.AND. spc(mfornec,mvalor-dupr->valor,,)
                                                @ lci+9,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B' .AND. lim_get() WHEN men_get(lci+10,cci+31,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
                                                @ lci+10,cci+27 GET mcod_ven PICT "999" VALID ven(mcod_ven,lci+10,cci+31) .AND. lim_get() WHEN men_get(lci+11,cci+31,'Informe o Cod. do vendedor "CAMPO NAO OBRIGATORIO"')
                                                @ lci+11,cci+27 GET mnum_ped PICT "999999" VALID lim_get() WHEN men_get(lci+12,cci+31,'Informe o No.do pedido que corresponde a este documento "CAMPO NAO OBRIGATORIO"')
                                                @ lci+12,cci+27 GET mcomissao PICT "999.99" VALID lim_get() WHEN men_get(lci+13,cci+31,'Informe o percentual de comissao do vendedor para este documento')
                                                @ lci+13,cci+27 GET mcorrente PICT "@!" VALID lim_get() WHEN men_get(lci+14,cci+31,'Informe o nome do correntista')
                                                @ lci+14,cci+27 GET mtip_pg PICT "@!" VALID lim_get() WHEN men_get(lci+15,cci+31,'Informe o Tipo de Operacao')
                                                @ lci+15,cci+07 GET mobs
                                                READ
                                                IF LASTKEY()=27
                                                        RESTSCREEN(00,00,24,79,mtel)
                                                        LOOP
                                                ENDIF
                                                Mensagem('Confirma os Dados ? [S/n]:')
                                                opcao := 'S'
                                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                                READ
                                                IF LASTKEY() = 27 .OR. opcao = "N"
                                                        RESTSCREEN(00,00,24,79,mtel)
                                                        LOOP
                                                ENDIF
                                                GO m_pos[point]
                                                IF BLOQREG()
                                                        dupr-> tipo := mtipo
                                                        IF EMPTY(mnum_banco) .OR. mtipo = 'DU'
                                                                dupr-> numero := SPACE(3)
                                                        ELSE
                                                                dupr-> numero := STRZERO(mnum_banco,3)
                                                        ENDIF
                                                        dupr-> agencia := magencia
                                                        dupr-> c_c := mc_c
                                                        dupr-> duplicata := mduplicata
                                                        dupr-> tip_cli := cli->tipo
                                                        dupr-> fornec := STRZERO(mfornec,5)
                                                        dupr-> cliente := mnome_cli
                                                        dupr-> emissao := memissao
                                                        dupr-> venc := mvenc
                                                        dupr-> valor := mvalor
                                                        dupr-> vendedor := STRZERO(mcod_ven,3)
                                                        dupr-> num_ped := STRZERO(mnum_ped,6)
                                                        dupr-> banco := mbanco
                                                        dupr-> comissao := mcomissao
                                                        dupr-> corrente := mcorrente
                                                        dupr-> tip_pg   := mtip_pg
                                                        dupr-> alt_oper := cod_operado
                                                        dupr-> obs      := mobs
                                                        IF memissao = mvenc
                                                                mensagem('Deseja fazer a BAIXA deste documento agora [S/n]:')
                                                                opcao := 'S'
                                                                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                                                READ
                                                                IF LASTKEY() = 27
                                                                        RESTSCREEN(00,00,24,79,mtel)
                                                                        LOOP
                                                                ENDIF
                                                                IF opcao = 'S'
                                                                        dupr->datpag := mvenc
                                                                        dupr->vlpago := mvalor
                                                                        dupr->pago := "B"
                                                                        dupr->banco := 'C'
                                                                ENDIF
                                                        ENDIF
                                                        dupr-> operador := cod_operado
                                                        IF cx <> NIL
                                                                dupr->mov_cx := 'C'
                                                        ELSE
                                                                dupr->mov_cx := 'M'
                                                        ENDIF
                                                        COMMIT
                                                        UNLOCK
                                                ELSE
                                                        atencao('Nao foi possivel Acessar o Arquivo !!!')
                                                        RESTSCREEN(00,00,24,79,mtel)
                                                        LOOP
                                                ENDIF
*                                               dias_atras(mfornec)    // CALULAR DIAS EM ATRASO P/DUPLICATA

                                                *****************
                                                SELE('dupr');ORDSETFOCUS(1)
                                                *****************
                                                DBUNLOCKALL()
                                                DBCOMMITALL()
                                                IF ver_serie() = '975976'
                                                        mensagem('Deseja Imprimir o RECIBO [S/n]:')
                                                        opcao := 'S'
                                                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                                        READ
                                                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                                                RESTSCREEN(00,00,24,79,mtel)
                                                                LOOP
                                                        ENDIF
                                                        sac63(mnome_cli,mvalor,mvenc)
                                                        LOOP
                                                ELSE
                                                        mensagem('Deseja Imprimir a DUPLICATA [S/n]:')
                                                        opcao := 'S'
                                                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                                        READ
                                                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                                                RESTSCREEN(00,00,24,79,mtel)
                                                                LOOP
                                                        ENDIF
                                                ENDIF
                                                IF mcgc_firm = '16.314.569/0001-61'
                                                        SET DEVI TO PRINT
                                                        SET PRINT TO (cfg->prt_dup)
                                                        imprt(cfg->imp_dup,'C')
                                                        @ PROW()+10,21 SAY mduplicata
                                                        DEVPOS(PROW(),40);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                                                        @ PROW(),60 SAY mduplicata
                                                        DEVPOS(PROW(),80);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                                                        @ PROW(),99 SAY mvenc
                                                        @ PROW()+3,65 SAY STRZERO(mvenc-mdata_sis,3)+' dias'
                                                        @ PROW()+3,60 SAY STRZERO(mfornec,5)
                                                        @ PROW(),PCOL()+1 SAY mnome_cli
                                                        @ PROW()+1,60 SAY cli->endereco
                                                        @ PROW()+1,60 SAY RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)
                                                        @ PROW(),110 SAY cli->uf
                                                        @ PROW()+1,60 SAY 'Cobranca Bancaria'
                                                        IF cli->cgc <> SPACE(14)
                                                                DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                                                                DEVPOS(PROW(),100);DEVOUT(cli->insc)
                                                        ELSEIF cli->cpf <> SPACE(11)
                                                                DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cpf,'@@R 999.999.999-99')
                                                        ELSE
                                                                DEVPOS(PROW()+1,00);DEVOUT(' ')
                                                        ENDIF
*                                                       mextenso:= '('+extenso(ROUND(mvalor,2),.T.)+')'
                                                        mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2)))+')'
                                                        mlinha := MLCOUNT(mextenso,50)
                                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                                        FOR lin = 1 TO mlinha
                                                                mlin := MEMOLINE(mextenso,50,lin,,10)
                                                                DEVPOS(PROW()+1,60);DEVOUT(PADR(RTRIM(mlin),50))
                                                        NEXT
                                                        imprt(cfg->imp_dup,'N')
                                                ELSEIF ver_serie() = '141236'
                                                        SET DEVI TO PRINT
                                                        SET PRINT TO (cfg->prt_dup)
                                                        dup_sot(dupr->emissao,dupr->operador,dupr->valor_dup,dupr->duplicata,dupr->venc,;
                                                                dupr->cliente,mfornec,cli->endereco,cli->bairro,cli->cidade,;
                                                                cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf)
                                                ELSE
                                                        SET DEVI TO PRINT
                                                        SET PRINT TO (cfg->prt_dup)
                                                        imprt(cfg->imp_dup,'E')
                                                        pitalico(.T.)
                                                        DEVPOS(PROW(),00);DEVOUT(memp_resa)
                                                        pitalico(.F.)
                                                        imprt(cfg->imp_dup,'N',1)
                                                        DEVPOS(PROW(),00);DEVOUT(mend_firm)
                                                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                                                        DEVPOS(PROW()+1,00);DEVOUT('Fone: '+mfone_firm)
                                                        DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
                                                        @ PROW()+1,00 SAY mtraco
                                                        @ PROW()+1,00 SAY ' '
                                                        @ PROW(),00 SAY 'Cliente:'
                                                        @ PROW(),PCOL()+1 SAY STRZERO(mfornec,5)
                                                        @ PROW(),PCOL()+1 SAY mnome_cli
                                                        @ PROW()+1,00 SAY 'End.: '+cli->endereco
                                                        @ PROW()+1,00 SAY 'Bairro: '+RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)+'-'+cli->uf+' - '+cli->cep
                                                        IF cli->cgc <> SPACE(14)
                                                                @ PROW()+1,00 SAY 'CGC:'
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Insc.: '+cli->insc)
                                                        ELSEIF cli->cpf <> SPACE(11)
                                                                @ PROW()+1,00 SAY 'CPF:'
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cpf,'@@R 999.999.999-99')
                                                        ENDIF
                                                        @ PROW()+1,00 SAY mtraco
                                                        imprt(cfg->imp_dup,'E',1)
                                                        pitalico(.T.)
                                                        @ PROW(),00 SAY 'DUPLICATA No.: '+mtipo+mduplicata
                                                        pitalico(.F.)
                                                        imprt(cfg->imp_dup,'N')
                                                        @ PROW()+1,00 SAY mtraco
                                                        @ PROW()+1,00 SAY 'Emissao:'
                                                        @ PROW(),09 SAY memissao
                                                        @ PROW(),38 SAY 'Vencimento:'
                                                        @ PROW(),50 SAY mvenc
                                                        @ PROW(),50 SAY mvenc
                                                        @ PROW()+1,00 SAY 'Fatura No.:_______________'
                                                        @ PROW(),49 SAY 'Cod.:'
                                                        @ PROW(),55 SAY STRZERO(mcod_ven,3)
                                                        @ PROW()+1,00 SAY mtraco
                                                        DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
                                                        DEVPOS(PROW(),00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
*                                                       mextenso:= '('+extenso(mvalor,.T.)+')'
                                                        mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2)))+')'
                                                        mlinha := MLCOUNT(mextenso,80)
                                                        FOR lin = 1 TO mlinha
                                                                mlin := MEMOLINE(mextenso,80,lin,,10)
                                                                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),80))
                                                        NEXT
                                                        @ PROW()+1,00 SAY mtraco
                                                        mextenso:= 'Reconheco(cemos) a exatidao desta DUPLICATA DE VENDA MERCANTIL, na importancia acima que pagarei(emos) a '+memp_resa+', ou a sua ordem na praca e vencimento indicados.'
                                                        mlinha := 0
                                                        mlinha := MLCOUNT(mextenso,80)
                                                        lin := 0
                                                        FOR lin = 1 TO mlinha
                                                                mlin := MEMOLINE(mextenso,80,lin,,10)
                                                                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(mlin))
                                                        NEXT
                                                        @ PROW()+1,00 SAY mtraco
                                                        DEVPOS(PROW()+2,00);DEVOUT(PADC('Em ________/________/________',80))
                                                        DEVPOS(PROW()+1,00);DEVOUT(PADC('Data do Aceite',80))
                                                        DEVPOS(PROW()+3,00);DEVOUT(PADC('_________________________________           _______________________________',80))
                                                        DEVPOS(PROW()+1,00);DEVOUT(PADC('     Assinatura do Emitente                      Assinatura do Sacado',80))
                                                        @ PROW()+1,00 SAY mtraco
                                                ENDIF
                                                EJECT
                                                SETPRC(00,00)
                                                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                                                RESTSCREEN(00,00,24,79,mtel)
                                        ENDIF
                        ENDCASE
                ELSE
                        atencao('Nao existe nenhum Documento a ser baixado')
                        LOOP
                ENDIF
        ELSE
                IF mtp = NIL .OR. mtp = 'A'
                        SEEK mtipo+mnumero+mduplicata+' '
                        IF ! FOUND()
                                atencao('Documento nao Cadastrado ou PAGO !!!')
                                LOOP
                        ENDIF
                ELSEIF mtp = 'S'
                        SEEK mtipo+mnumero+mduplicata+'B'
                        IF ! FOUND()
                                atencao('Documento nao Cadastrado ou NAO FOI PAGO !!!')
                                LOOP
                        ENDIF
                ENDIF
                mcod_cli := VAL(fornec)
                mforn_aux := VAL(fornec)
                Mensagem('Digite o Codigo do cliente ou [F7]p/consultar. [ESC] Abandona.')
                @ lc+1,cc+28 GET mcod_cli PICT '99999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF mcod_cli <> mforn_aux
                        GO TOP
                        SEEK mtipo+mnumero+mduplicata+' '+STRZERO(mcod_cli,5)
                        IF ! FOUND()
                                atencao('Nao existe esta duplicata ou ja foi PAGA !!!')
                                LOOP
                        ENDIF
                ENDIF
                mjuros := 0
                mmulta := 0
                mdatpag=mdata_sis
                mvlpago=valor
                mbanco := SUBSTR(banco,1,1)
                mcomissao := comissao
                mobs = dupr->obs
                IF ver_serie() = '141304'
                        mdias_atras := mdatpag-(dupr->venc+set->dia_venc)
                ELSE
                        mdias_atras := mdatpag-dupr->venc
                ENDIF
                IF mdias_atras > 0
                        mdias_atras := mdatpag-dupr->venc
                        mmulta := dupr->valor * ((set->multa_dup)/100)
*                       mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                        mjuros := (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                ENDIF
                mvlpago=valor+mjuros+mmulta
                setcor(3)
                @ lc+1,cc+35 SAY dupr->cliente
                @ lc+4,cc+28 SAY mduplicata+conta
                @ lc+5,cc+28 SAY dupr->emissao
                @ lc+6,cc+28 SAY dupr->venc
                @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                @ lc+9,cc+28 SAY dupr->valor PICT "99,999,999.99"
                @ lc+10,cc+28 SAY mmulta PICT "99,999,999.99"
                @ lc+11,cc+28 SAY mjuros PICT "99,999,999.99"
                setcor(1)
                IF mtp = NIL
                        @ lc+8,cc+28 GET mdatpag
                        READ
                        IF LASTKEY()=27
                                LOOP
                        ENDIF
                        IF ver_serie() = '141304'
                                mdias_atras := mdatpag-(dupr->venc+set->dia_venc)
                        ELSE
                                mdias_atras := mdatpag-dupr->venc
                        ENDIF
                        IF mdias_atras > 0
                                mdias_atras := mdatpag-dupr->venc
                                mmulta := dupr->valor * ((set->multa_dup)/100)
                                mjuros := (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                        ELSE
                                mjuros := 0
                                mmulta := 0
                        ENDIF
                        mvlpago=valor+mjuros+mmulta
                        setcor(3)
                        @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                        @ lc+10,cc+28 SAY mmulta PICT "99,999,999.99"
                        @ lc+11,cc+28 SAY mjuros PICT "99,999,999.99"
                        setcor(1)
                        @ lc+12,cc+28 GET mvlpago PICT "99,999,999.99"
                        @ lc+13,cc+28 GET mtip_pg PICT '@!'
                        @ lc+14,cc+28 GET mcheque PICT '99999999'
                        @ lc+15,cc+07 GET mobs
                        READ
                        IF LASTKEY()=27
                                LOOP
                        ENDIF
                ELSEIF mtp = 'A'
                        mtel := SAVESCREEN(00,00,24,79)
                        lci := 03
                        cci := 00
                        lba := 19
                        cba := 79
                        botao(lci,cci,lba,cba)
                        @ lci+1,cci+1 TO lci+1,cba-1
                        DEVPOS(lci,cci+1);DEVOUT(" Contas a Receber ")
                        DEVPOS(lci+2,cci+2);DEVOUT('Tipo Documentos........:')
                        DEVPOS(lci+3,cci+2);DEVOUT('No.Banco/Cod.Cartao....:')
                        DEVPOS(lci+3,cci+32);DEVOUT('Agencia:')
                        DEVPOS(lci+3,cci+51);DEVOUT('C/C:')
                        DEVPOS(lci+4,cci+2);DEVOUT('Numero Documentos......:')
                        DEVPOS(lci+5,cci+2);DEVOUT('Codigo do Cliente......:')
                        DEVPOS(lci+6,cci+2);DEVOUT('Data de Emissao........:')
                        DEVPOS(lci+7,cci+2);DEVOUT('Data de Vencimento.....:')
                        DEVPOS(lci+8,cci+2);DEVOUT('Valor Documento R$.....:')
                        DEVPOS(lci+9,cci+2);DEVOUT('Pag. [C]arteria [B]anco:')
                        DEVPOS(lci+10,cci+2);DEVOUT('Codigo do Vendedor.....:')
                        DEVPOS(lci+11,cci+2);DEVOUT('Numero do Pedido/NF....:')
                        DEVPOS(lci+12,cci+2);DEVOUT('% de Comissao .........:')
                        DEVPOS(lci+13,cci+2);DEVOUT('Nome do Correntista....:')
                        DEVPOS(lci+14,cci+2);DEVOUT('Tipo de Operacao.......:')
                        DEVPOS(lci+15,cci+2);DEVOUT('OBS.:')
                        point := RECNO()
                        mfornec=VAL(dupr->fornec)
                        mnome_cli := dupr->cliente
                        memissao=dupr->emissao
                        mvenc=dupr->venc
                        mvalor=dupr->valor
                        mtipo=dupr->tipo
                        mcod_ven := VAL(dupr->vendedor)
                        mnum_ped := VAL(dupr->num_ped)
                        mbanco := SUBSTR(dupr->banco,1,1)
                        mnum_banco := VAL(dupr->numero)
                        mcomissao := dupr->comissao
                        magencia := dupr->agencia
                        mc_c := dupr->c_c
                        mcorrente := dupr->corrente
                        mobs := dupr->obs
                        mtip_pg := dupr->tip_pg
                        mensagem('Complete os Dados. [ESC] Abandona.')
                        IF RTRIM(mnome_cli) = 'DIVERSOS'
                                mnome_cli := dupr->cliente
                                setcor(3)
                                DEVPOS(lci+5,cci+32);DEVOUT(mnome_cli)
                                setcor(1)
                        ENDIF
                        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,CT,FI,RE' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <CT>cartao <FI>financiamento <RE>recebimentos')
                        @ lci+3,cci+27 GET mnum_banco PICT '999' VALID lim_get() WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+31,'Informe o No.do banco')
                        @ lci+3,cci+41 GET magencia PICT '@!' VALID lim_get() WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+41,'Informe a AGENCIA BANCARIA')
                        @ lci+3,cci+56 GET mc_c PICT '@!' VALID lim_get() .AND. qtd_chq(mfornec,STRZERO(mnum_banco,3),magencia,mc_c) WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+56,'Informe a C/C BANCARIA')
                        @ lci+4,cci+27 GET mduplicata PICT '@!' VALID lim_get() WHEN men_get(lci+5,cci+31,'Informe o No.do documento')
                        @ lci+5,cci+27 GET mfornec PICT "99999" VALID mcli(mfornec,lci+5,cci+27) .AND. lim_get() WHEN men_get(lci+6,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
                        READ
                        IF LASTKEY()=27
                                RESTSCREEN(00,00,24,79,mtel)
                                LOOP
                        ENDIF
                        mnome_cli := dupr->cliente
                        IF RTRIM(mnome_cli) = 'DIVERSOS'
                                @ lci+5,cci+32 GET mnome_cli PICT '@!' VALID lim_get() WHEN men_get(lci+6,cci+23,'Neste campo voce pode descrever o que deseja')
                        ENDIF
                        @ lci+6,cci+27 GET memissao
                        @ lci+7,cci+27 GET mvenc VALID IF(EMPTY(mvenc),.F.,.T.)
                        @ lci+8,cci+27 GET mvalor PICT "@E 99,999,999,999.99" VALID mvalor>0 .AND. spc(mfornec,mvalor-dupr->valor,,)
                        @ lci+9,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B' .AND. lim_get() WHEN men_get(lci+10,cci+31,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
                        @ lci+10,cci+27 GET mcod_ven PICT "999" VALID ven(mcod_ven,lci+10,cci+31) .AND. lim_get() WHEN men_get(lci+11,cci+31,'Informe o Cod. do vendedor "CAMPO NAO OBRIGATORIO"')
                        @ lci+11,cci+27 GET mnum_ped PICT "999999" VALID lim_get() WHEN men_get(lci+12,cci+31,'Informe o No.do pedido que corresponde a este documento "CAMPO NAO OBRIGATORIO"')
                        @ lci+12,cci+27 GET mcomissao PICT "999.99" VALID lim_get() WHEN men_get(lci+13,cci+31,'Informe o percentual de comissao do vendedor para este documento')
                        @ lci+13,cci+27 GET mcorrente PICT "@!" VALID lim_get() WHEN men_get(lci+14,cci+31,'Informe o nome do correntista')
                        @ lci+14,cci+27 GET mtip_pg PICT "@!" VALID lim_get() WHEN men_get(lci+15,cci+31,'Informe o Tipo de Operacao')
                        @ lci+15,cci+07 GET mobs
                        READ
                        IF LASTKEY()=27
                                RESTSCREEN(00,00,24,79,mtel)
                                LOOP
                        ENDIF
                        Mensagem('Confirma os Dados ? [S/n]:')
                        opcao := 'S'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = "N"
                                RESTSCREEN(00,00,24,79,mtel)
                                LOOP
                        ENDIF
                        GO point
                        IF BLOQREG()
                                dupr-> tipo := mtipo
                                IF EMPTY(mnum_banco) .OR. mtipo = 'DU'
                                        dupr-> numero := SPACE(3)
                                ELSE
                                        dupr-> numero := STRZERO(mnum_banco,3)
                                ENDIF
                                dupr-> agencia := magencia
                                dupr-> c_c := mc_c
                                dupr-> duplicata := mduplicata
                                dupr-> tip_cli := cli->tipo
                                dupr-> fornec := STRZERO(mfornec,5)
                                dupr-> cliente := mnome_cli
                                dupr-> emissao := memissao
                                dupr-> venc := mvenc
                                dupr-> valor := mvalor
                                dupr-> vendedor := STRZERO(mcod_ven,3)
                                dupr-> num_ped := STRZERO(mnum_ped,6)
                                dupr-> banco := mbanco
                                dupr-> comissao := mcomissao
                                dupr-> corrente := mcorrente
                                dupr-> tip_pg   := mtip_pg
                                dupr-> alt_oper := cod_operado
                                dupr-> obs      := mobs
                                IF memissao = mvenc
                                        mensagem('Deseja fazer a BAIXA deste documento agora [S/n]:')
                                        opcao := 'S'
                                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                        READ
                                        IF LASTKEY() = 27
                                                RESTSCREEN(00,00,24,79,mtel)
                                                LOOP
                                        ENDIF
                                        IF opcao = 'S'
                                                dupr->datpag := mvenc
                                                dupr->vlpago := mvalor
                                                dupr->pago := "B"
                                                dupr->banco := 'C'
                                        ENDIF
                                ENDIF
                                dupr-> operador := cod_operado
                                IF cx <> NIL
                                        dupr->mov_cx := 'C'
                                ELSE
                                        dupr->mov_cx := 'M'
                                ENDIF
                                COMMIT
                                UNLOCK
                        ELSE
                                atencao('Nao foi possivel Acessar o Arquivo !!!')
                                RESTSCREEN(00,00,24,79,mtel)
                                LOOP
                        ENDIF
*                       dias_atras(mfornec)    // CALULAR DIAS EM ATRASO P/DUPLICATA

                        *****************
                        SELE('dupr');ORDSETFOCUS(1)
                        *****************
                        DBUNLOCKALL()
                        DBCOMMITALL()
                        IF ver_serie() = '975976'
                                mensagem('Deseja Imprimir o RECIBO [S/n]:')
                                opcao := 'S'
                                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        RESTSCREEN(00,00,24,79,mtel)
                                        LOOP
                                ENDIF
                                sac63(mnome_cli,mvalor,mvenc)
                                LOOP
                        ELSE
                                mensagem('Deseja Imprimir a DUPLICATA [S/n]:')
                                opcao := 'S'
                                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        RESTSCREEN(00,00,24,79,mtel)
                                        LOOP
                                ENDIF
                        ENDIF
                        IF mcgc_firm = '16.314.569/0001-61'
                                SET DEVI TO PRINT
                                SET PRINT TO (cfg->prt_dup)
                                imprt(cfg->imp_dup,'C')
                                @ PROW()+10,21 SAY mduplicata
                                DEVPOS(PROW(),40);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                                @ PROW(),60 SAY mduplicata
                                DEVPOS(PROW(),80);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                                @ PROW(),99 SAY mvenc
                                @ PROW()+3,65 SAY STRZERO(mvenc-mdata_sis,3)+' dias'
                                @ PROW()+3,60 SAY STRZERO(mfornec,5)
                                @ PROW(),PCOL()+1 SAY mnome_cli
                                @ PROW()+1,60 SAY cli->endereco
                                @ PROW()+1,60 SAY RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)
                                @ PROW(),110 SAY cli->uf
                                @ PROW()+1,60 SAY 'Cobranca Bancaria'
                                IF cli->cgc <> SPACE(14)
                                        DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                                        DEVPOS(PROW(),100);DEVOUT(cli->insc)
                                ELSEIF cli->cpf <> SPACE(11)
                                        DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cpf,'@@R 999.999.999-99')
                                ELSE
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
*                               mextenso:= '('+extenso(ROUND(mvalor,2),.T.)+')'
                                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2)))+')'
                                mlinha := MLCOUNT(mextenso,50)
                                DEVPOS(PROW()+1,00);DEVOUT(' ')
                                FOR lin = 1 TO mlinha
                                        mlin := MEMOLINE(mextenso,50,lin,,10)
                                        DEVPOS(PROW()+1,60);DEVOUT(PADR(RTRIM(mlin),50))
                                NEXT
                                imprt(cfg->imp_dup,'N')
                        ELSEIF ver_serie() = '141236'
                                SET DEVI TO PRINT
                                SET PRINT TO (cfg->prt_dup)
                                dup_sot(dupr->emissao,dupr->operador,dupr->valor_dup,dupr->duplicata,dupr->venc,;
                                        dupr->cliente,mfornec,cli->endereco,cli->bairro,cli->cidade,;
                                        cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf)
                        ELSEIF ver_serie() = '141258'
                                SET DEVI TO PRINT
                                SET PRINT TO (set->dup_port)
                                dup_sam(dupr->emissao,dupr->operador,dupr->valor_dup,dupr->duplicata,dupr->venc,;
                                        dupr->cliente,mfornec,cli->endereco,cli->bairro,cli->cidade,;
                                        cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf)
                        ELSE
                                SET DEVI TO PRINT
                                SET PRINT TO (cfg->prt_dup)
                                imprt(cfg->imp_dup,'E')
                                pitalico(.T.)
                                DEVPOS(PROW(),00);DEVOUT(memp_resa)
                                pitalico(.F.)
                                imprt(cfg->imp_dup,'N',1)
                                DEVPOS(PROW(),00);DEVOUT(mend_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Fone: '+mfone_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
                                @ PROW()+1,00 SAY mtraco
                                @ PROW()+1,00 SAY ' '
                                @ PROW(),00 SAY 'Cliente:'
                                @ PROW(),PCOL()+1 SAY STRZERO(mfornec,5)
                                @ PROW(),PCOL()+1 SAY mnome_cli
                                @ PROW()+1,00 SAY 'End.: '+cli->endereco
                                @ PROW()+1,00 SAY 'Bairro: '+RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)+'-'+cli->uf+' - '+cli->cep
                                IF cli->cgc <> SPACE(14)
                                        @ PROW()+1,00 SAY 'CGC:'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Insc.: '+cli->insc)
                                ELSEIF cli->cpf <> SPACE(11)
                                        @ PROW()+1,00 SAY 'CPF:'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cpf,'@@R 999.999.999-9')
                                ENDIF
                                @ PROW()+1,00 SAY mtraco
                                imprt(cfg->imp_dup,'E',1)
                                pitalico(.T.)
                                @ PROW(),00 SAY 'DUPLICATA No.: '+mtipo+mduplicata
                                pitalico(.F.)
                                imprt(cfg->imp_dup,'N')
                                @ PROW()+1,00 SAY mtraco
                                @ PROW()+1,00 SAY 'Emissao:'
                                @ PROW(),09 SAY memissao
                                @ PROW(),38 SAY 'Vencimento:'
                                @ PROW(),50 SAY mvenc
                                @ PROW(),50 SAY mvenc
                                @ PROW()+1,00 SAY 'Fatura No.:_______________'
                                @ PROW(),49 SAY 'Cod.:'
                                @ PROW(),55 SAY STRZERO(mcod_ven,3)
                                @ PROW()+1,00 SAY mtraco
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
                                DEVPOS(PROW(),00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
*                               mextenso:= '('+extenso(mvalor,.T.)+')'
                                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2)))+')'
                                mlinha := MLCOUNT(mextenso,80)
                                FOR lin = 1 TO mlinha
                                        mlin := MEMOLINE(mextenso,80,lin,,10)
                                        DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),80))
                                NEXT
                                @ PROW()+1,00 SAY mtraco
                                mextenso:= 'Reconheco(cemos) a exatidao desta DUPLICATA DE VENDA MERCANTIL, na importancia acima que pagarei(emos) a '+memp_resa+', ou a sua ordem na praca e vencimento indicados.'
                                mlinha := 0
                                mlinha := MLCOUNT(mextenso,80)
                                lin := 0
                                FOR lin = 1 TO mlinha
                                        mlin := MEMOLINE(mextenso,80,lin,,10)
                                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(mlin))
                                NEXT
                                @ PROW()+1,00 SAY mtraco
                                DEVPOS(PROW()+2,00);DEVOUT(PADC('Em ________/________/________',80))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Data do Aceite',80))
                                DEVPOS(PROW()+3,00);DEVOUT(PADC('_________________________________           _______________________________',80))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('     Assinatura do Emitente                      Assinatura do Sacado',80))
                                @ PROW()+1,00 SAY mtraco
                        ENDIF
                        EJECT
                        SETPRC(00,00)
                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        RESTSCREEN(00,00,24,79,mtel)
                        LOOP
                ELSEIF mtp = 'S'
                        setcor(3)
                        @ lc+7,cc+28 SAY IF(mdias_atras<0,TRANSFORM(0,'9999'),TRANSFORM(mdias_atras,'9999'))
                        @ lc+8,cc+28 SAY dupr->datpag
                        @ lc+10,cc+28 SAY "             "
                        @ lc+11,cc+28 SAY dupr->juros PICT "99,999,999.99"
                        @ lc+12,cc+28 SAY dupr->vlpago PICT "99,999,999.99"
                        @ lc+13,cc+28 SAY dupr->tip_pg PICT '99999999'
                        @ lc+14,cc+28 SAY dupr->cheque PICT '99999999'
                        setcor(1)
                ENDIF
        ENDIF
        IF mtp = NIL
                Mensagem('Confirma os Dados ? [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                IF mvlpago < valor+mjuros+mmulta
                        mensagem('Este Valor vai ser por CONTA [S/n]:')
                        pconta := 'S'
                        @ MAXROW()-1,COL()+1 GET pconta PICT '@!' VALID pconta $ 'S,N'
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        IF pconta = 'N'
                                IF ! aut_sen('Senha de autorizacao p/dispensa o restante do documento:','LIB_RECE')
                                        LOOP
                                ENDIF
                        ENDIF
                ENDIF
                ASIZE(m_dup,0)
                ASIZE(m_po,0)
                IF BLOQREG()
                        dupr->datpag := mdatpag
                        dupr->vlpago := mvlpago
                        dupr->cheque := mcheque
                        IF pconta = 'S'
                                pconta := '*'
                                mcliente := dupr->cliente
                                mvalor_dup := dupr->valor_dup
                                mvalor_cont := (dupr->valor+mmulta+mjuros)-mvlpago
                                memissao := dupr->emissao
                                mvenc := dupr->venc
                                mcod_vend := dupr->vendedor
                                mcomissao := dupr->comissao
                                mnum_ped := dupr->num_ped
                                mtip_cli := dupr->tip_cli
                                mnumero := dupr->numero
                                dupr->valor := mvlpago
                                dupr->conta := pconta
                        ELSE
                                IF mvlpago<valor
                                        dupr->desc := valor-mvlpago
                                ELSE
                                        dupr->juros := mvlpago-valor
                                ENDIF
                        ENDIF
                        dupr->pago := "B"
                        dupr->tip_pg := mtip_pg
                        dupr->banco := mbanco
                        dupr->operador := cod_operado
                        dupr->obs := mobs
                        IF cx <> NIL
                                dupr->mov_cx := 'C'
                        ELSE
                                dupr->mov_cx := 'M'
                        ENDIF
                        AADD(m_dup,dupr->fornec+' '+dupr->tipo+' '+dupr->duplicata+' '+DTOC(dupr->venc)+' '+TRANSFORM(dupr->valor,'999,999.99')+' '+TRANSFORM(mjuros+mmulta,'999,999.99')+' '+TRANSFORM(mvlpago,'999,999.99')+' X')
                        AADD(m_po,{RECNO(),'X',dupr->valor,mjuros+mmulta,mvlpago,'X'})
                        DBCOMMITALL()
                        DBUNLOCKALL()
                ELSE
                        atencao('Nao foi possivel acessar o Arquivo !!!')
                        LOOP
                ENDIF
                IF pconta = '*'
                        IF ADIREG()
                                dupr-> tipo := mtipo
                                IF mtipo = 'CH'
                                        dupr-> numero := mnumero
                                ENDIF
                                dupr-> duplicata := mduplicata
                                dupr-> numero  := mnumero
                                dupr-> valor_dup := mvalor_dup
                                dupr-> fornec := STRZERO(mcod_cli,5)
                                dupr-> cliente := mcliente
                                dupr-> tip_cli := mtip_cli
                                dupr-> emissao := memissao
                                IF ! EMPTY(mmulta) .OR. ! EMPTY(mjuros)
                                        dupr-> venc := mdatpag
                                ELSE
                                        dupr-> venc := mvenc
                                ENDIF
                                dupr-> valor := mvalor_cont
                                dupr-> pago := " "
                                dupr-> vendedor := mcod_vend
                                dupr-> num_ped := mnum_ped
                                dupr-> conta := pconta
                                dupr-> banco := mbanco
                                dupr-> comissao := mcomissao
                                dupr-> operador := cod_operado
                                dupr->obs := mobs
                        ENDIF
                ENDIF
                IF cx <> NIL
                        ******************
                        SELE('caix');ORDSETFOCUS(3)
                        GO TOP
                        *****************
                        IF mtipo = 'DU'

                                SEEK mtipo + mduplicata + STRZERO(mcod_cli,5)

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
                        IF ADIREG()
                                caix-> tipo := mtipo
                                caix-> num_dup := mduplicata
                                caix-> documento := mduplicata
                                caix-> data := mdatpag
                                caix-> venci := mdatpag
                                caix-> valor := mvlpago
                                caix-> cod_cli := dupr->fornec
                                caix-> nota := mnota
                                caix-> tipo_comp := mtipo_comp
                                caix-> cod_vend := mcod_vend
                                caix-> cod_opera := cod_operado
                                caix-> pg := '*'
                                caix-> op_pg := 'P'
                                caix-> valor_com := mvlpago
                                caix-> comissao := mcomissao
                                DBCOMMIT()
                                DBUNLOCK()
                        ELSE
                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                LOOP
                        ENDIF
                ENDIF
                IF mtipo = 'CH'
                        ******************
                        SELE('caix');ORDSETFOCUS(3)
                        GO TOP
                        *****************

                        SEEK mtipo + mduplicata + STRZERO(mcod_cli,5)

                        IF FOUND()
                                mnota := nota
                                mtipo_comp := tipo_comp
                                mcod_vend := cod_vend
                                IF BLOQREG()
                                        caix->op_pg := 'P'
                                        caix-> pg := '*'
                                        DBCOMMIT()
                                        DBUNLOCK()
                                ELSE
                                        atencao('Nao foi possivel acessar o Arquivo !!!')
                                        LOOP
                                ENDIF
                        ENDIF
                ENDIF
                IF cfg->aut_dup = 'R' .OR. cfg->aut_dup = 'A'
                        IF  cfg->aut_dup = 'R'
                                mtipo_aut := 2
                        ELSE
                                mtipo_aut := 1
                        ENDIF
                        mtela_aut := SAVESCREEN(00,00,24,79)
                        botao(10,35,12,67,,' Opcao de Autenticacoes ')
                        @ 11,36 PROMPT ' Autenticar '
                        @ 11,50 PROMPT ' Imprimir Recibo '
                        SET INTEN ON
                        MENU TO mtipo_aut
                        IF  mtipo_aut = 2
                                RESTSCREEN(00,00,24,79,mtela_aut)
                                rec_dupr(m_dup,m_po,1,mjuros+mmulta,mvlpago,mdatpag,mtp)
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
                                        SET PRINT TO (cfg->prt_dup)
                                        imprt(cfg->imp_dup,'C')
                                        DEVPOS(00,00);DEVOUT(ALLTRIM(SUBSTR(memp_resa,1,4))+ALLTRIM(mduplicata)+cod_operado+DTOC(mdatpag)+ALLTRIM(TRANSFORM(mvlpago,'999999.99')))
                                        imprt(cfg->imp_dup,'N')
                                        SETPRC(00,00)
                                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                                ENDDO
                                RESTSCREEN(00,00,24,79,mtela_aut)
                        ENDIF
                ENDIF
                Mensagem('Deseja gerar um CREDITO em CONTA CORRENTE [S/n]:')
                opcao := 'N'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        *****************
                        SELE('dupr');ORDSETFOCUS(1)
                        *****************
                        LOOP
                ENDIF
                IF opcao = 'S'
                        sac2cc11(mvlpago,'RECEBIMENTO DA DUPLICATA DE No.: '+mtipo+mduplicata)
                ENDIF
        ELSEIF mtp = 'S'
                Mensagem('Confirma o Estorno do DOCUMENTO ? [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                IF ! BLOQREG()
                        atencao('Nao foi possivel acessar o registro..')
                        LOOP
                ENDIF
                dupr->datpag := CTOD('  /  /  ')
                dupr->vlpago := 0
                dupr->pago := ' '
                dupr->tip_pg := '  '
                dupr->desc := 0
                dupr->juros := 0
                dupr->cheque := SPACE(8)
                dupr->operador := cod_operado
                dupr->obs := mobs
                DBUNLOCK()
                DBCOMMIT()
                IF dupr->mov_cx = 'C'
                        ******************
                        SELE('caix');ORDSETFOCUS(3)
                        GO TOP
                        *****************

                        SEEK mtipo + mduplicata + STRZERO(mcod_cli,5) + '*'
                        IF FOUND()
                                BLOQREG()
                                DELE
                                DBUNLOCK()
                                DBCOMMIT()
                        ENDIF
                ENDIF
        ENDIF
        *****************
        SELE('dupr');ORDSETFOCUS(1)
        *****************
        DBCOMMIT()
ENDDO

