*** CON211.PRG: Inclusao de Duplicatas (a pagar)
***
************************************************

FUNCTION con211
****************
LOCAL MPRG:='CON211'
LOCAL mtipo,mduplicata,mcod_ven,mtraco,opcao,tela,mobs,mvalor

PRIVATE mcod_forn,mvenc,memissao,mnome_vend,mcom_ven,mfornece,mend,mbairro,mcidade,;
        muf,mcgc,mcpf,minsc,mbanco,mnum_nf

IF ! ver_nivel(mprg,'INCLUSAO DE DOCUMENTOS (C.APAGAR)','15')
        RETURN NIL
ENDIF

lci := 09
cci := 00
lba := 20
cba := 78
mtraco := REPLI('-',80)
@ lci,cci+2 SAY " Contas a Pagar "
janela(lci,cba," Inclusao de Documentos ")
WHILE .T.
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        *****************
        mfornece := SPACE(40)
        mend := SPACE(35)
        mbairro := SPACE(20)
        mcidade := SPACE(20)
        muf := SPACE(2)
        mcgc := SPACE(14)
        mcpf := SPACE(11)
        minsc := SPACE(14)
        mcod_ven := 0
        mnome_vend := SPACE(30)
        mcom_ven := 0
        mduplicata=SPACE(10)
        mcod_forn=0
        memissao=mdata_sis
        mvenc=CTOD("  /  /  ")
        mvalor=0.00
        mtipo=SPACE(2)
        mnum_nf := SPACE(6)
        mbanco := SPACE(1)
        mobs := SPACE(60)
        Mensagem('Digite o Numero da Documento. [ESC] Abandona.')
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,FR,PA' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <FR>frete <PA>pagamentos')
        @ lci+3,cci+27 GET mduplicata PICT '@!' VALID IF(EMPTY(mduplicata),.F.,.T.) .AND. lim_get() WHEN men_get(lci+4,cci+31,'Informe o No.do documento')
        @ lci+4,cci+27 GET mcod_forn PICT "9999" VALID IF(EMPTY(mcod_forn),.F.,v_fornece(mcod_forn,lci+4,cci+32)) .AND. lim_get() WHEN men_get(lci+5,cci+31,'Informe o Cod. do Fornecedor ou <F6>p/pesquisar')
        READ
        IF LASTKEY()=27 .OR. mduplicata=SPACE(10)
                RETURN NIL
        ENDIF
        Mensagem('Complete os Dados. [ESC] Abandona.')
/*
        IF mfornece = 'DIVERSOS'
                @ lci+4,cci+32 GET mfornece PICT "@!" VALID lim_get() WHEN men_get(lci+5,cci+31,'Campo liberado para qualquer descricao')
        ENDIF
*/
        mfornece := forn->razao
        IF forn->tipo = 'FV' .OR. forn->tipo = 'FI' .OR. forn->tipo = 'VV' .OR. forn->tipo = 'AD' .OR. forn->tipo = 'TR' .OR. forn->tipo = 'DV'
                @ lci+4,cci+32 GET mfornece PICT "@!" VALID lim_get() WHEN men_get(lci+5,cci+31,'Campo liberado para qualquer descricao')
        ENDIF
        @ lci+5,cci+27 GET memissao
        @ lci+6,cci+27 GET mvenc VALID vencim(mvenc)
        @ lci+7,cci+27 GET mvalor PICT "99,999,999,999.99" VALID IF(forn->tipo = 'FV' .OR. forn->tipo = 'FI' .OR. forn->tipo = 'VV' .OR. forn->tipo = 'AD' .OR. forn->tipo = 'TR' .OR. forn->tipo = 'DV',.T.,IF(mvalor<0,.F.,.T.))
        @ lci+8,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B, ' .AND. lim_get() WHEN men_get(lci+9,cci+31,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
        @ lci+9,cci+27 GET mnum_nf PICT "@!" VALID lim_get() WHEN men_get(lci+10,cci+31,'Digite o numero da NOTA FISCAL')
        @ lci+10,cci+14 GET mobs
        READ
        SEEK mtipo+mduplicata+' '+STRZERO(mcod_forn,4)+DTOS(mvenc)
        IF FOUND()
                IF mvalor = valor
                        IF mtipo = 'DU'
                                atencao('Esta Documento ja existe e esta em ABERTA !')
                        ELSE
                                atencao('Este Frete ja existe e esta em ABERTO !')
                        ENDIF
                        Mensagem('Deseja Incluir mesmo assim ? [S/n]:')
                        opcao := 'N'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = "N"
                                LOOP
                        ENDIF
                ENDIF
        ENDIF
        SEEK mtipo+mduplicata+'B'+STRZERO(mcod_forn,4)+DTOS(mvenc)
        IF FOUND()
                IF mvalor = valor
                        IF mtipo = 'DU'
                                atencao('Esta Documento ja existe e ja foi PAGA !')
                        ELSE
                                atencao('Este Frete ja existe e ja foi PAGO !')
                        ENDIF
                        Mensagem('Deseja Incluir mesmo assim ? [S/n]:')
                        opcao := 'N'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = "N"
                                LOOP
                        ENDIF
                ENDIF
        ENDIF
        IF LASTKEY()=27
                RETURN NIL
        ENDIF
        Mensagem('Confirma os Dados ? [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = "N"
                LOOP
        ENDIF
        IF ADIREG()
                dupp-> tipo := mtipo
                dupp-> duplicata := mduplicata
                dupp-> valor_dup := mvalor
                dupp-> tip_for := forn->tipo
                dupp-> fornec := STRZERO(mcod_forn,4)
                dupp-> cliente := mfornece
                dupp-> emissao := memissao
                dupp-> venc := mvenc
                dupp-> valor := mvalor
                dupp-> pago := " "
                dupp-> banco := mbanco
                dupp-> vendedor := cod_operado
                dupp-> num_ped := mnum_nf
                IF memissao = mvenc
                        mensagem('Deseja fazer a BAIXA deste documento agora [S/n]:')
                        opcao := 'S'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        IF opcao = 'S'
                                dupp->datpag := mvenc
                                dupp->vlpago := mvalor
                                dupp->pago := "B"
                                dupp->banco := 'C'
                        ENDIF
                ENDIF
                dupp-> obs      := mobs
                dupp-> operador := cod_operado
        ELSE
                atencao('Nao foi possivel Acessar o Arquivo !!!')
                LOOP
        ENDIF
        **************
        SELE('dupp');ORDSETFOCUS(1)
        **************
        DBUNLOCKALL()
        DBCOMMITALL()
        mensagem('Deseja Imprimir o DOCUMENTO [S/n]:')
        opcao := 'N'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        SET DEVI TO PRINT
        SET PRINT TO (set->relatorio)
        imprt(set->impress,'E')
        pitalico(.T.)
        DEVPOS(PROW(),00);DEVOUT(memp+resa)
        pitalico(.F.)
        imprt(set->impress,'N',1)
        DEVPOS(PROW(),00);DEVOUT(mend_firm)
        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
        DEVPOS(PROW()+1,00);DEVOUT('Fone: '+mfone_firm)
        DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
        @ PROW()+1,00 SAY mtraco
        @ PROW()+1,00 SAY ' '
        @ PROW(),00 SAY 'Despesa:'
        @ PROW(),PCOL()+1 SAY STRZERO(mcod_forn,4)
        @ PROW(),PCOL()+1 SAY mfornece
        @ PROW()+1,00 SAY 'End.: '+mend
        @ PROW()+1,00 SAY 'Bairro: '+RTRIM(mbairro)+'-Cidade: '+RTRIM(mcidade)+'-'+muf
        IF mcgc <> SPACE(14)
        @ PROW()+1,00 SAY 'CGC:'
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(mcgc,'@@R 99.999.999/9999-99')
                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Insc.: '+minsc)
        ELSEIF mcpf <> SPACE(11)
                @ PROW()+1,00 SAY 'CPF:'
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(mcpf,'@@R 999.999.999-99')
        ENDIF
        @ PROW()+1,00 SAY mtraco
        imprt(set->impress,'E',1)
        pitalico(.T.)
        @ PROW(),00 SAY 'DUPLICATA A PAGAR No.: '+mtipo+mduplicata
        pitalico(.F.)
        imprt(set->impress,'N')
        @ PROW()+1,00 SAY mtraco
        @ PROW()+1,00 SAY 'Emissao:'
        @ PROW(),09 SAY memissao
        @ PROW(),38 SAY 'Vencimento:'
        @ PROW(),50 SAY mvenc
        @ PROW(),50 SAY mvenc
        @ PROW(),PCOL()+2 SAY 'Cod.:'
        @ PROW(),PCOL()+1 SAY cod_operado
        @ PROW()+1,00 SAY mtraco
        DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
*       mextenso:= '('+extenso(mvalor,.T.)+')'
        mextenso:= '('+RTRIM(EXT(mvalor,10))+')'
        mlinha := MLCOUNT(mextenso,80)
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,80,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),80))
        NEXT
        @ PROW()+1,00 SAY mtraco
        DEVPOS(PROW()+4,00);DEVOUT(PADC('Em ________/________/________',80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('Data do Aceite',80))
        DEVPOS(PROW()+3,00);DEVOUT(PADC('_________________________________           _______________________________',80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('     Assinatura do Emitente                      Assinatura do Sacado',80))
        @ PROW()+1,00 SAY mtraco
        EJECT
        SETPRC(00,00)
        SET PRINT TO
        SET DEVI TO SCREEN
ENDDO
*********************** f i m *************************************
