*** CON2111.PRG: Inclusao de VARIAS Duplicatas (Receber)
***
************************************************

FUNCTION con2111
****************
LOCAL MPRG:='CON2111'
LOCAL mtipo,mduplicata,mtraco,opcao,tela,mvalor,;
      mobs,mqtd_parc:=0,mintervalo:=0,mdoc,i,tela1
LOCAL lci,cci,lba,cba

PRIVATE mcod_forn,mvenc,memissao,mnome_vend,mcom_ven,mnome_cli,mend,mbairro,mcidade,;
        muf,mcgc,mcpf,minsc,mbanco,mnum_banco,mtipo_imp,mdesc_cart,mprazo_cart,;
        mcartao,m_dup:={},mcodconta,mfornece

IF ! ver_nivel(mprg,'CONTA A PAGAR (INCLUSAO DE VARIOS DOCUMENTO)','15',nivel_acess)
        RETURN NIL
ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('sacdupp','dupp');RETURN NIL;ENDIF
IF ! AbriArq('sacforn','forn');RETURN NIL;ENDIF

lci := 03
cci := 00
lba := 18
cba := 79

li := 03
ci := 00
lb := 20
cb := 79

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('-',80)
WHILE .T.
        ASIZE(m_dup,0)
        exibi_prg(mprg)
        botao(lci,cci,lba,cba,,' INCLUSAO Documentos A PAGAR ')
        DEVPOS(lci+2,cci+2);DEVOUT('Codigo Fornec./Despesa.:')
        DEVPOS(lci+3,cci+2);DEVOUT('Tipo Documentos........:')
        DEVPOS(lci+4,cci+2);DEVOUT('Tipo de Conta..........:')
        DEVPOS(lci+5,cci+2);DEVOUT('Numero Documentos......:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data de Emissao........:')
        DEVPOS(lci+7,cci+2);DEVOUT('Valor Total R$.........:')
        DEVPOS(lci+8,cci+2);DEVOUT('Pag. [C]arteria [B]anco:')
        DEVPOS(lci+9,cci+2);DEVOUT('OBS.:')
        DEVPOS(lci+10,cci+2);DEVOUT('Qtd. de Parcelas:')
        DEVPOS(lci+10,cci+25);DEVOUT('Intervalos:')
        mtipo_imp := cfg->imp_rel
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        GO TOP
        *****************
        mduplicata=SPACE(7)
        mcod_forn=0
        memissao=mdata_sis
        mvenc=CTOD("  /  /  ")
        mvalor=0
        mtipo=SPACE(2)
        mcodconta := '    '
        mobs := SPACE(60)
        mbanco := ' '
        Mensagem('Complete os Dados.  - <F1> Ajuda do Campo - [ESC] Abandona.')
        @ lci+2,cci+27 GET mcod_forn PICT "9999" VALID IF(EMPTY(mcod_forn),.F.,v_fornece(mcod_forn,lci+2,cci+32))
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela1)
                RETURN NIL
        ENDIF
        mcodconta := forn->tipo
        mfornece := forn->razao
        @ lci+3,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'PA,DU,FR,CH' .AND. lim_get() WHEN men_get(lci+4,cci+27,'Informe o tipo de documento <DU>duplicata <CH>cheques <FR>frete <PA>pagamentos')
        @ lci+4,cci+27 GET mcodconta PICT '@! X.XX' VALID verdesp(mcodconta,lci+4,cci+32) .AND. lim_get() WHEN men_get(lci+5,cci+27,'Identifique o Grupo da Conta preenchendo so o primeiro digito da conta, ou preenchendo todo os espaco p/o tipo de conta <F1>Ajuda')
        READ
        lim_get()
        setcor(3)
        DEVPOS(lci+4,cci+27);DEVOUT(mcodconta)
        setcor(1)
        verdesp(mcodconta,lci+4,cci+32)
        @ lci+5,cci+27 GET mduplicata PICT '@!'
        @ lci+6,cci+27 GET memissao
        @ lci+7,cci+27 GET mvalor PICT "9,999,999.99" VALID mvalor>0
        @ lci+8,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B' .AND. lim_get() WHEN men_get(lci+9,cci+27,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO')
        @ lci+9,cci+07 GET mobs
        @ lci+10,cci+20 GET mqtd_parc PICT '99' VALID IF(mqtd_parc > 30,.F.,.T.)
        @ lci+10,cci+37 GET mintervalo PICT '99'
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
        mparcela := mvalor / mqtd_parc
        i := 0
        mvenc := mdata_sis
        FOR i = 1 TO 30
                IF i <= mqtd_parc
                        mdoc := ALLTRIM(mduplicata)+'/'+STRZERO(i,2)+SPACE(11-LEN(ALLTRIM(mduplicata)+'/'+STRZERO(i,2)))
                        mvenc := mvenc + mintervalo
                        IF DOW(mvenc) = 1
                                mvenc := mvenc + 1
                        ENDIF
                        AADD(m_dup,{mparcela,mvenc,mdoc})
                ELSE
                        AADD(m_dup,{0,CTOD('  /  /  '),SPACE(11)})
                ENDIF
        NEXT

        tela1 := SAVESCREEN(01,00,24,79)
        botao(li,ci,lb,cb,,' DEMONSTRATIVO DE DOCUMENTOS ')
        @ li+2,ci+1 GET m_dup[1,3] PICT '@!'
        @ li+2,ci+14 GET m_dup[1,2]
        @ li+2,ci+24 GET m_dup[1,1] PICT '999,999.99'

        @ li+3,ci+1 GET m_dup[2,3] PICT '@!'
        @ li+3,ci+14 GET m_dup[2,2]
        @ li+3,ci+24 GET m_dup[2,1] PICT '999,999.99'

        @ li+4,ci+1 GET m_dup[3,3] PICT '@!'
        @ li+4,ci+14 GET m_dup[3,2]
        @ li+4,ci+24 GET m_dup[3,1] PICT '999,999.99'

        @ li+5,ci+1 GET m_dup[4,3] PICT '@!'
        @ li+5,ci+14 GET m_dup[4,2]
        @ li+5,ci+24 GET m_dup[4,1] PICT '999,999.99'

        @ li+6,ci+1 GET m_dup[5,3] PICT '@!'
        @ li+6,ci+14 GET m_dup[5,2]
        @ li+6,ci+24 GET m_dup[5,1] PICT '999,999.99'

        @ li+7,ci+1 GET m_dup[6,3] PICT '@!'
        @ li+7,ci+14 GET m_dup[6,2]
        @ li+7,ci+24 GET m_dup[6,1] PICT '999,999.99'

        @ li+8,ci+1 GET m_dup[7,3] PICT '@!'
        @ li+8,ci+14 GET m_dup[7,2]
        @ li+8,ci+24 GET m_dup[7,1] PICT '999,999.99'

        @ li+9,ci+1 GET m_dup[8,3] PICT '@!'
        @ li+9,ci+14 GET m_dup[8,2]
        @ li+9,ci+24 GET m_dup[8,1] PICT '999,999.99'

        @ li+10,ci+1 GET m_dup[9,3] PICT '@!'
        @ li+10,ci+14 GET m_dup[9,2]
        @ li+10,ci+24 GET m_dup[9,1] PICT '999,999.99'

        @ li+11,ci+1 GET m_dup[10,3] PICT '@!'
        @ li+11,ci+14 GET m_dup[10,2]
        @ li+11,ci+24 GET m_dup[10,1] PICT '999,999.99'

        @ li+12,ci+1 GET m_dup[11,3] PICT '@!'
        @ li+12,ci+14 GET m_dup[11,2]
        @ li+12,ci+24 GET m_dup[11,1] PICT '999,999.99'

        @ li+13,ci+1 GET m_dup[12,3] PICT '@!'
        @ li+13,ci+14 GET m_dup[12,2]
        @ li+13,ci+24 GET m_dup[12,1] PICT '999,999.99'

        @ li+14,ci+1 GET m_dup[13,3] PICT '@!'
        @ li+14,ci+14 GET m_dup[13,2]
        @ li+14,ci+24 GET m_dup[13,1] PICT '999,999.99'

        @ li+15,ci+1 GET m_dup[14,3] PICT '@!'
        @ li+15,ci+14 GET m_dup[14,2]
        @ li+15,ci+24 GET m_dup[14,1] PICT '999,999.99'

        @ li+16,ci+1 GET m_dup[15,3] PICT '@!'
        @ li+16,ci+14 GET m_dup[15,2]
        @ li+16,ci+24 GET m_dup[15,1] PICT '999,999.99'

        @ li+2,ci+45 GET m_dup[16,3] PICT '@!'
        @ li+2,ci+58 GET m_dup[16,2]
        @ li+2,ci+68 GET m_dup[16,1] PICT '999,999.99'

        @ li+3,ci+45 GET m_dup[17,3] PICT '@!'
        @ li+3,ci+58 GET m_dup[17,2]
        @ li+3,ci+68 GET m_dup[17,1] PICT '999,999.99'

        @ li+4,ci+45 GET m_dup[18,3] PICT '@!'
        @ li+4,ci+58 GET m_dup[18,2]
        @ li+4,ci+68 GET m_dup[18,1] PICT '999,999.99'

        @ li+5,ci+45 GET m_dup[19,3] PICT '@!'
        @ li+5,ci+58 GET m_dup[19,2]
        @ li+5,ci+68 GET m_dup[19,1] PICT '999,999.99'

        @ li+6,ci+45 GET m_dup[20,3] PICT '@!'
        @ li+6,ci+58 GET m_dup[20,2]
        @ li+6,ci+68 GET m_dup[20,1] PICT '999,999.99'

        @ li+7,ci+45 GET m_dup[21,3] PICT '@!'
        @ li+7,ci+58 GET m_dup[21,2]
        @ li+7,ci+68 GET m_dup[21,1] PICT '999,999.99'

        @ li+8,ci+45 GET m_dup[22,3] PICT '@!'
        @ li+8,ci+58 GET m_dup[22,2]
        @ li+8,ci+68 GET m_dup[22,1] PICT '999,999.99'

        @ li+9,ci+45 GET m_dup[23,3] PICT '@!'
        @ li+9,ci+58 GET m_dup[23,2]
        @ li+9,ci+68 GET m_dup[23,1] PICT '999,999.99'

        @ li+10,ci+45 GET m_dup[24,3] PICT '@!'
        @ li+10,ci+58 GET m_dup[24,2]
        @ li+10,ci+68 GET m_dup[24,1] PICT '999,999.99'

        @ li+11,ci+45 GET m_dup[25,3] PICT '@!'
        @ li+11,ci+58 GET m_dup[25,2]
        @ li+11,ci+68 GET m_dup[25,1] PICT '999,999.99'

        @ li+12,ci+45 GET m_dup[26,3] PICT '@!'
        @ li+12,ci+58 GET m_dup[26,2]
        @ li+12,ci+68 GET m_dup[26,1] PICT '999,999.99'

        @ li+13,ci+45 GET m_dup[27,3] PICT '@!'
        @ li+13,ci+58 GET m_dup[27,2]
        @ li+13,ci+68 GET m_dup[27,1] PICT '999,999.99'

        @ li+14,ci+45 GET m_dup[28,3] PICT '@!'
        @ li+14,ci+58 GET m_dup[28,2]
        @ li+14,ci+68 GET m_dup[28,1] PICT '999,999.99'

        @ li+15,ci+45 GET m_dup[29,3] PICT '@!'
        @ li+15,ci+58 GET m_dup[29,2]
        @ li+15,ci+68 GET m_dup[29,1] PICT '999,999.99'

        @ li+16,ci+45 GET m_dup[30,3] PICT '@!'
        @ li+16,ci+58 GET m_dup[30,2]
        @ li+16,ci+68 GET m_dup[30,1] PICT '999,999.99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela1)
                LOOP
        ENDIF
        Mensagem('Confirma os Dados ? [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = "N"
                LOOP
        ENDIF
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        GO TOP
        *****************
        i := 0
        FOR i = 1 TO mqtd_parc
                IF ADIREG()
                        dupp-> tipo := mtipo
                        dupp-> duplicata := m_dup[i,3]
                        dupp-> valor_dup := m_dup[i,1]
                        dupp-> tip_for := mcodconta
                        dupp-> fornec := STRZERO(mcod_forn,5)
                        dupp-> cliente := mfornece
                        dupp-> emissao := memissao
                        dupp-> venc := m_dup[i,2]
                        dupp-> valor := m_dup[i,1]
                        dupp-> pago := " "
                        dupp-> vendedor := cod_operado
                        dupp-> banco := mbanco
                        dupp-> obs      := mobs
                        dupp-> operador := cod_operado
                ELSE
                        atencao('Nao foi possivel Acessar o Arquivo !!!')
                        LOOP
                ENDIF
        NEXT
        DBUNLOCKALL()
        DBCOMMITALL()
        RESTSCREEN(01,00,24,79,tela1)
ENDDO
*********************** f i m *************************************




