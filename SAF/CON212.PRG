*** CON212.PRG: Alteracao de Duplicatas (a pagar)
***
*************************************************

FUNCTION con212
****************
LOCAL MPRG:='CON212'
LOCAL tela,mduplicata,lfor,last,mvalor,mtipo,mcod_ven,mnum_nf,opcao,mtraco,point,;
      mobs

PRIVATE mcod_forn,mvenc,memissao,mnome_vend,mcom_ven,mfornece,mend,mbairro,mcidade,;
        muf,mcgc,mcpf,minsc,mbanco

IF ! ver_nivel(mprg,'ALTERACAO DE DOCUMENTOS (C.APAGAR)','15')
        RETURN NIL
ENDIF

mtraco := REPLI('-',80)
*****************
SELE('dupp');ORDSETFOCUS(1)
*****************
@ lci,cci+2 SAY " Contas a Pagar "
janela(lci,cba," Alteracao de Documentos ")
setcor(1)
save scree to tela242
WHILE .T.
        rest scree from tela242
        mcod_ven := 0
        mnome_vend := SPACE(30)
        mcom_ven := 0
        mduplicata=SPACE(10)
        mtipo=SPACE(2)
        mnum_nf := SPACE(6)
        mcod_forn := 0
        mbanco := SPACE(1)
        mfornece := SPACE(40)
        mend := SPACE(40)
        mobs := SPACE(60)
        Mensagem("Digite o Numero da Documento. [ESC] Abandona.")
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,FR,PA' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <FR>Frete e <PA>pagamento')
        @ lci+3,cci+27 GET mduplicata PICT '@!' VALID IF(EMPTY(mduplicata),.F.,.T.) .AND. lim_get() WHEN men_get(lci+4,cci+31,'Informe o No.do documento')
        @ lci+4,cci+27 GET mcod_forn PICT "9999" VALID IF(EMPTY(mcod_forn),.F.,v_fornece(mcod_forn,lci+4,cci+32)) .AND. lim_get() WHEN men_get(lci+5,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
        READ
        IF LASTKEY()=27
                RETURN
        ENDIF
        SEEK mtipo+mduplicata+' '+STRZERO(mcod_forn,4)
        IF ! FOUND() .OR. pago = 'B'
                atencao('Este Documento ja foi pago ou nao existe !!!')
                LOOP
        ENDIF
        point := RECNO()
        mensagem('Complete os Dados. [ESC] Abandona.')
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,FR,PA' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <FR>frete e <PA>pagamento')
        @ lci+3,cci+27 GET mduplicata PICT '@!' VALID lim_get() WHEN men_get(lci+4,cci+31,'Informe o No.do documento')
        @ lci+4,cci+27 GET mcod_forn PICT "9999" VALID IF(EMPTY(mcod_forn),.F.,v_fornece(mcod_forn,lci+4,cci+32)) .AND. lim_get() WHEN men_get(lci+5,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
/*
        SEEK mtipo+mduplicata+' '+STRZERO(mcod_forn,4)
        IF FOUND() .AND. pago = 'B'
                atencao('Este Documento ja foi pago !!!')
                Mensagem('Deseja Continuar ? [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = "N"
                        LOOP
                ENDIF
        ELSEIF FOUND()
                atencao('Este Documento ja existe !!!')
                Mensagem('Deseja Continuar ? [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = "N"
                        LOOP
                ENDIF
        ELSE
                GO point
        ENDIF
*/
        lfor=fornec
        last=valor
        memissao=emissao
        mvenc=venc
        mvalor=valor
        mcod_ven := VAL(vendedor)
        mnum_nf := num_ped
        mbanco := SUBSTR(banco,1,1)
        mobs := dupp->obs
        IF forn->tipo = 'FV' .OR. forn->tipo = 'FI' .OR. forn->tipo = 'VV' .OR. forn->tipo = 'AD' .OR. forn->tipo = 'TR' .OR. forn->tipo = 'DV'
                mfornece := forn->razao
                @ lci+4,cci+32 GET mfornece PICT "@!" VALID lim_get() WHEN men_get(lci+5,cci+31,'Campo liberado para qualquer descricao')
        ENDIF
        @ lci+5,cci+27 GET memissao
        @ lci+6,cci+27 GET mvenc VALID IF(EMPTY(mvenc),.F.,.T.)
        @ lci+7,cci+27 GET mvalor PICTure "@E 99,999,999,999.99" VALID mvalor>0
        @ lci+8,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B, ' .AND. lim_get() WHEN men_get(lci+9,cci+31,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
        @ lci+9,cci+27 GET mnum_nf PICT "@!" VALID lim_get() WHEN men_get(lci+10,cci+31,'Digite o numero da NOTA FISCAL')
        @ lci+10,cci+14 GET mobs
        READ
        IF LASTKEY()=27
                RETURN
        ENDIF
        Mensagem('Confirma os Dados ? [S/n]:')
        opcao := 'S'
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = "N"
                LOOP
        ENDIF
        IF BLOQREG()
                dupp-> tipo := mtipo
                dupp-> duplicata := mduplicata
                dupp-> tip_for := forn->tipo
                dupp-> fornec := STRZERO(mcod_forn,4)
                dupp-> cliente := forn->razao
                dupp-> emissao := memissao
                dupp-> venc := mvenc
                dupp-> valor := mvalor
                dupp-> vendedor := cod_operado
                dupp-> banco := mbanco
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
                dupp-> operador := cod_operado
                dupp-> obs      := mobs
                COMMIT
                UNLOCK
        ELSE
                atencao('Nao foi possivel Acessar o Arquivo !!!')
                LOOP
        ENDIF
        *****************
        SELE('dupp');ORDSETFOCUS(1)
        *****************
        DBUNLOCKALL()
        DBCOMMITALL()
        mensagem('Deseja Imprimir a DOCUMENTO [S/n]:')
        opcao := 'S'
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
        @ PROW(),00 SAY 'Fornecedor:'
        @ PROW(),PCOL()+1 SAY STRZERO(mcod_forn,4)
        @ PROW(),PCOL()+1 SAY forn->razao
        @ PROW()+1,00 SAY mtraco
        imprt(set->impress,'E',1)
        pitalico(.T.)
        @ PROW(),00 SAY 'DUPLICATA No.: '+mtipo+mduplicata
        pitalico(.F.)
        imprt(set->impress,'N')
        @ PROW()+1,00 SAY mtraco
        @ PROW()+1,00 SAY 'Emissao:'
        @ PROW(),09 SAY memissao
        @ PROW(),38 SAY 'Vencimento:'
        @ PROW(),50 SAY mvenc
        @ PROW(),50 SAY mvenc
        @ PROW()+1,00 SAY 'Fatura No.:_______________'
        @ PROW(),49 SAY 'Cod.:'
        @ PROW(),55 SAY cod_operado
        @ PROW()+1,00 SAY mtraco
        DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
        DEVPOS(PROW(),00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
*       mextenso:= '('+extenso(mvalor,.T.)+')'
        mextenso:= '('+RTRIM(EXT(mvalor))+')'
        mlinha := MLCOUNT(mextenso,80)
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,80,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),80))
        NEXT
        @ PROW()+1,00 SAY mtraco
        mextenso:= 'Reconheco(cemos) a exatidao desta DUPLICATA DE VENDA MERCANTIL, na importancia acima que pagarei(emos) a '+memp+resa+', ou a sua ordem na praca e vencimento indicados.'
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
        EJECT
        SETPRC(00,00)
        SET PRINT TO
        SET DEVI TO SCREEN
ENDDO
*******************************  F I M  **********************************

