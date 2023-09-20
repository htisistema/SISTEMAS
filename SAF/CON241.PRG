*** CON241.PRG: Inclusao de Duplicatas (Receber)
***
************************************************

FUNCTION con241
****************
LOCAL MPRG:='CON241'
LOCAL mtipo,mduplicata,mcod_ven,mtraco,opcao,mnum_ped,tela,mcomissao,mvalor,;
      magencia,mc_c,mcorrente,mobs

PRIVATE mcod_cli,mvenc,memissao,mnome_vend,mcom_ven,mnome_cli,mend,mbairro,mcidade,;
        muf,mcgc,mcpf,minsc,mbanco,mnum_banco,mtipo_imp,mdesc_cart,mprazo_cart,;
        mcartao

IF ! ver_nivel(mprg,'CONTA A RECEBER (INCLUSAO DE DOCUMENTO)','15',nivel_acess)
        RETURN NIL
ENDIF

mtraco := REPLI('-',80)
@ lci,cci+2 SAY " Contas a Receber "
janela(lci,cba," Inclusao de Duplicatas ")
WHILE .T.
        mtipo_imp := cfg->imp_rel
        *****************
        SELE('dupr');ORDSETFOCUS(1)
        GO TOP
        *****************
        mnome_cli := SPACE(40)
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
        mcod_cli=0
        memissao=mdata_sis
        mvenc=CTOD("  /  /  ")
        mvalor=0
        mtipo=SPACE(2)
        mnum_ped := 0
        mnum_banco := 0
        mbanco := 'C'
        mcomissao := 0
        magencia := SPACE(8)
        mc_c := SPACE(13)
        mcorrente := SPACE(35)
        mobs := SPACE(60)
        mdesc_cart := 0
        mprazo_cart:= 0
        mcartao := SPACE(20)
        Mensagem('Digite o Numero da Duplicata. - <F1> Ajuda - [ESC] Abandona.')
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID IF(mtipo='CT',lim_get() .AND. limpa(lci+3,cci+32,lci+3,cba-1),mtipo $ 'CH,DU,CT,FI,RE' .AND. lim_get()) WHEN men_get(lci+3,cci+25,'Informe o tipo de documento <DU>duplicata <CH>cheques <CT>cartao <FI>financiamento e <RE>recebimento')
        @ lci+3,cci+27 GET mnum_banco PICT '999' VALID IF(mtipo='CT',ver_cartao(mnum_banco,lci+3,cci+31,'*'),.T.) .AND. lim_get() WHEN mtipo <> 'DU' .AND. men_get(lci+4,cci+25,'Informe o No.do banco ou se tipo de documento for "CT" informe o codigo do cartao')
        @ lci+3,cci+41 GET magencia PICT '@!' VALID lim_get() WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+41,'Informe a AGENCIA BANCARIA')
        @ lci+3,cci+56 GET mc_c PICT '@!' VALID lim_get()  WHEN mtipo = 'CH' .AND. men_get(lci+4,cci+56,'Informe a C/C BANCARIA')
        @ lci+4,cci+27 GET mduplicata PICT '@!' VALID lim_get() WHEN men_get(lci+5,cci+25,'Informe o No.do documento')
        READ
        IF LASTKEY()=27 .OR. mduplicata=SPACE(10)
                RETURN NIL
        ENDIF
        Mensagem('Complete os Dados. [ESC] Abandona.')
        @ lci+5,cci+27 GET mcod_cli PICT "99999"                //VALID lim_get() WHEN men_get(lci+6,cci+25,'Informe o Cod. do Cliente ou <ENTER>p/pesquisar p/NOME - <F1>Ajuda')
        @ lci+5,cci+33 GET mnome_cli PICT "@!" WHEN EMPTY(mcod_cli)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF ! EMPTY(mcod_cli)
                *****************
                SELE('cli');ORDSETFOCUS(1)
                *****************

                IF ! cli->(DBSEEK(STRZERO(mcod_cli,5)))
                        atencao('Cliente nao cadastrado !!!')
                        LOOP
                ENDIF
        ELSEIF ! EMPTY(mnome_cli)
                *****************
                SELE('cli');ORDSETFOCUS(2)
                *****************

                IF ! cli->(DBSEEK(RTRIM(mnome_cli)))
                        atencao('Cliente nao cadastrado !!!')
                        LOOP
                ELSE
                        mcod_cli:=f7_cli()
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                ENDIF
        ENDIF
        *****************
        SELE('dupr');ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! mcli(mcod_cli,lci+5,cci+27)
                LOOP
        ENDIF
        qtd_chq(mcod_cli,STRZERO(mnum_banco,3),magencia,mc_c)
        IF mnum_banco = 0
                SEEK mtipo+SPACE(3)+mduplicata+' '+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
        ELSE
                SEEK mtipo+STRZERO(mnum_banco,3)+mduplicata+' '+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
        ENDIF
        IF FOUND()
                IF mtipo = 'CH'
                        atencao('Este Cheque ja existe esta em ABERTO !')
                ELSEIF mtipo = 'DU'
                        atencao('Esta Duplicata ja existe esta em ABERTO !')
                ELSEIF mtipo = 'FI'
                        atencao('Este Financiamento ja existe esta em ABERTO!')
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
        IF mnum_banco = 0
                SEEK mtipo+SPACE(3)+mduplicata+'B'+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
        ELSE
                SEEK mtipo+STRZERO(mnum_banco,3)+mduplicata+'B'+STRZERO(mcod_cli,5)         //+DTOS(mvenc)
        ENDIF
        IF FOUND()
                IF mtipo = 'CH'
                        atencao('Este Cheque ja existe e ja foi PAGO !')
                ELSEIF mtipo = 'DU'
                        atencao('Esta Duplicata ja existe e ja foi PAGA !')
                ELSEIF mtipo = 'FI'
                        atencao('Este Financiamento ja existe e ja foi PAGO !')
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
        IF RTRIM(mnome_cli) = 'DIVERSOS'
                @ lci+5,cci+32 GET mnome_cli PICT '@!' VALID lim_get() WHEN men_get(lci+6,cci+23,'Neste campo voce pode descrever o que deseja')
        ENDIF
        mvenc := mdata_sis + mprazo_cart
        @ lci+6,cci+27 GET memissao
        @ lci+7,cci+27 GET mvenc VALID vencim(mvenc)
        @ lci+8,cci+27 GET mvalor PICT "9,999,999.99" VALID mvalor>0 .AND. spc(mcod_cli,mvalor,,)
        @ lci+9,cci+27 GET mbanco PICT "@!" VALID mbanco $ 'C,B' .AND. lim_get() WHEN men_get(lci+10,cci+23,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
        @ lci+10,cci+27 GET mcod_ven PICT "999" VALID ven(mcod_ven,lci+10,cci+31) .AND. lim_get() WHEN men_get(lci+11,cci+31,'Informe o Cod. do vendedor "CAMPO NAO OBRIGATORIO"')
        @ lci+11,cci+27 GET mnum_ped PICT "999999" VALID lim_get() WHEN men_get(lci+12,cci+23,'Informe o No.do pedido que corresponde a este documento "CAMPO NAO OBRIGATORIO"')
        @ lci+12,cci+27 GET mcomissao PICT "999.9999" VALID lim_get() WHEN men_get(lci+13,cci+23,'Informe a comissao do vendedor em relacao a este documento')
        @ lci+13,cci+27 GET mcorrente PICT "@!" VALID lim_get() WHEN men_get(lci+14,cci+23,'Informe o nome do correntista')
        @ lci+14,cci+07 GET mobs
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
        IF ADIREG()
                dupr-> tipo := mtipo
                IF EMPTY(mnum_banco)
                        dupr-> numero := SPACE(3)
                ELSE
                        dupr-> numero := STRZERO(mnum_banco,3)
                ENDIF
                dupr-> agencia := magencia
                dupr-> c_c := mc_c
                dupr-> duplicata := mduplicata
                dupr-> valor_dup := mvalor
                dupr-> tip_cli := cli->tipo
                dupr-> fornec := STRZERO(mcod_cli,5)
                dupr-> cliente := mnome_cli
                dupr-> emissao := memissao
                dupr-> venc := mvenc
                IF mtipo = 'CT'
                        dupr-> valor := mvalor - (mvalor * (mdesc_cart/100))
                ELSE
                        dupr-> valor := mvalor
                ENDIF
                dupr-> pago := " "
                dupr-> vendedor := STRZERO(mcod_ven,3)
                dupr-> num_ped := STRZERO(mnum_ped,6)
                dupr-> banco := mbanco
                dupr-> comissao := mcomissao
                dupr-> corrente := mcorrente
                dupr-> obs      := mobs
                IF memissao = mvenc
                        mensagem('Deseja fazer a BAIXA deste documento agora [S/n]:')
                        opcao := 'S'
                        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27
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
        ELSE
                atencao('Nao foi possivel Acessar o Arquivo !!!')
                LOOP
        ENDIF
        **************
        SELE('dupr');ORDSETFOCUS(1)
        **************
        DBUNLOCKALL()
        DBCOMMITALL()
        IF ver_serie() = '975976'
                mensagem('Deseja Imprimir o RECIBO [S/n]:')
                opcao := 'S'
                @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                sac63(mnome_cli,mvalor,mvenc)
                LOOP
        ELSE
                IF ! imp_arq('DUPLICATA','D','N')
                        LOOP
                ENDIF
        ENDIF
        IF mcgc_firm = '16.314.569/0001-61'
                imprt(mtipo_imp,'C')
                @ PROW()+10,21 SAY mduplicata
                DEVPOS(PROW(),40);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                @ PROW(),60 SAY mduplicata
                DEVPOS(PROW(),80);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                @ PROW(),99 SAY mvenc
                @ PROW()+3,65 SAY STRZERO(mvenc-mdata_sis,3)+' dias'
                @ PROW()+3,60 SAY STRZERO(mcod_cli,5)
                @ PROW(),PCOL()+1 SAY cli->razao
                @ PROW()+1,60 SAY cli->endereco
                @ PROW()+1,60 SAY RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)
                @ PROW(),110 SAY cli->uf
                @ PROW()+1,60 SAY 'Cobranca Bancaria'
                IF mcgc <> SPACE(14)
                        DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                        DEVPOS(PROW(),100);DEVOUT(cli->insc)
                ELSEIF mcpf <> SPACE(11)
                        DEVPOS(PROW()+1,60);DEVOUTPICT(cli->cpf,'@@R 999.999.999-99')
                ELSE
                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                ENDIF
*               mextenso:= '('+extenso(ROUND(mvalor,2),.T.)+')'
                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2),10))+')'
                mlinha := MLCOUNT(mextenso,50)
                DEVPOS(PROW()+1,00);DEVOUT(' ')
                FOR lin = 1 TO mlinha
                        mlin := MEMOLINE(mextenso,50,lin,,10)
                        DEVPOS(PROW()+1,60);DEVOUT(PADR(RTRIM(mlin),50))
                NEXT
                imprt(mtipo_imp,'N')
        ELSEIF ver_serie() = '141236' // SOTINTAS
                dup_sot(dupr->emissao,dupr->operador,dupr->valor_dup,dupr->duplicata,dupr->venc,;
                        dupr->cliente,mcod_cli,cli->endereco,cli->bairro,cli->cidade,;
                        cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf)
        ELSEIF ver_serie() = '141258' // IND.SAMPAIO
                dup_sam(dupr->emissao,dupr->operador,dupr->valor_dup,dupr->duplicata,dupr->venc,;
                        dupr->cliente,mcod_cli,cli->endereco,cli->bairro,cli->cidade,;
                        cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf)
        ELSE
                imprt(mtipo_imp,'E')
                pitalico(.T.)
                DEVPOS(PROW(),00);DEVOUT(memp_resa)
                pitalico(.F.)
                imprt(mtipo_imp,'N',1)
                DEVPOS(PROW(),00);DEVOUT(mend_firm)
                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Fone: '+mfone_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
                @ PROW()+1,00 SAY mtraco
                @ PROW()+1,00 SAY ' '
                @ PROW(),00 SAY 'Cliente:'
                @ PROW(),PCOL()+1 SAY STRZERO(mcod_cli,5)
                @ PROW(),PCOL()+1 SAY mnome_cli
                @ PROW()+1,00 SAY 'End.: '+mend
                @ PROW()+1,00 SAY 'Bairro: '+RTRIM(mbairro)+'-Cidade: '+RTRIM(mcidade)+'-'+muf+'- Cep:'+cli->cep
                IF mcgc <> SPACE(14)
                @ PROW()+1,00 SAY 'CGC:'
                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(mcgc,'@@R 99.999.999/9999-99')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Insc.: '+minsc)
                ELSEIF mcpf <> SPACE(11)
                        @ PROW()+1,00 SAY 'CPF:'
                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(mcpf,'@@R 999.999.999-99')
                ENDIF
                @ PROW()+1,00 SAY mtraco
                imprt(mtipo_imp,'E',1)
                pitalico(.T.)
                @ PROW(),00 SAY 'DUPLICATA No.: '+mtipo+mduplicata
                pitalico(.F.)
                imprt(mtipo_imp,'N')
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
*               mextenso:= '('+extenso(mvalor,.T.)+')'
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
ENDDO
*********************** f i m *************************************




