*** CON428.PRG: Emissao de Duplicatas (Receber)
***
************************************************

FUNCTION con428(mcli,mdoc,mvlr,mped)
****************
LOCAL MPRG:='CON428'
LOCAL mtipo,mduplicata,mcod_ven,mtraco,opcao,mnum_ped,tela,mtela,i:=0

PRIVATE mfornec,mvenc,memissao,mnome_vend,mcom_ven,mnome_cli,mend,mbairro,mcidade,;
        muf,mcgc,mcpf,minsc,mbanco,lci,cci,lba,cba,mtipo_imp,mqtd_cop:=0

IF ! ver_nivel(mprg,'CONTA A RECEBER (IMPRESSAO DE DUPLICATAS)','12345',nivel_acess)
        RETURN NIL
ENDIF

lci := 08
cci := 00
lba := 20
cba := 79
mtela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('-',80)
IF mdoc = NIL
        *------------------------------------------------------------------------------------
        CLOSE ALL
        IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
        IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
        IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
        IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
        IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
        *------------------------------------------------------------------------------------
        mduplicata=SPACE(10)
        mfornec=0
        mvalor = 0
        mnum_ped := 0
ELSE
        mduplicata=mdoc
        mfornec=mcli
        mvalor = mvlr
        mnum_ped := mped
ENDIF
set_key('cliente')
WHILE .T.
        mtipo_imp := cfg->imp_dup
        mqtd_cop  := cfg->qtdcopdup
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 TO lci+1,cba-1
        DEVPOS(lci,cci+1);DEVOUT(" Emissao de DUPLICATAS ")
        DEVPOS(lci+2,cci+2);DEVOUT('Tipo Duplicata.................:')
        DEVPOS(lci+3,cci+2);DEVOUT('Numero Duplicata...............:')
        DEVPOS(lci+4,cci+2);DEVOUT('Codigo do Cliente..............:')
        DEVPOS(lci+5,cci+2);DEVOUT('Data de Emissao................:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data de Vencimento.............:')
        DEVPOS(lci+7,cci+2);DEVOUT('Valor Duplicata R$.............:')
        DEVPOS(lci+8,cci+2);DEVOUT('Pagamento em [C]arteria [B]anco:')
        DEVPOS(lci+9,cci+2);DEVOUT('Codigo do Vendedor.............:')
        DEVPOS(lci+10,cci+2);DEVOUT('Numero do Pedido...............:')
        DEVPOS(lci+11,cci+2);DEVOUT('Quantidade de Copias...........:')
        *****************
        SELE('dupr');ORDSETFOCUS(1)
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
        memissao=mdata_sis
        mvenc=CTOD("  /  /  ")
        mtipo=SPACE(2)
        mbanco := SPACE(1)
        Mensagem('Digite o Numero da Duplicata. [ESC] Abandona.')
        @ lci+2,cci+35 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,CT,FI,RE' .AND. lim_get() WHEN men_get(lci+3,cci+31,'Informe o tipo de documento <DU>duplicata <CH>cheques <CT>cartao <FI>financeira e <RE>recebimentos')
        @ lci+3,cci+35 GET mduplicata PICT '@!' VALID lim_get() WHEN men_get(lci+4,cci+31,'Informe o No.do documento')
        READ
        IF LASTKEY()=27 .OR. mduplicata=SPACE(10)
                RESTSCREEN(01,00,24,79,mtela)
                RETURN NIL
        ENDIF
        Mensagem('Complete os Dados. [ESC] Abandona.')
        @ lci+4,cci+35 GET mfornec PICT "99999" VALID mcli(mfornec,lci+4,cci+35) .AND. lim_get() WHEN men_get(lci+5,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
        @ lci+5,cci+35 GET memissao
        @ lci+6,cci+35 GET mvenc VALID vencim(mvenc)
        @ lci+7,cci+35 GET mvalor PICTure "99,999,999,999.99" VALID mvalor>0
        @ lci+8,cci+35 GET mbanco PICT "@!" VALID mbanco $ 'C,B, ' .AND. lim_get() WHEN men_get(lci+9,cci+31,'Digite [C] para pagamento em CARTEIRA [B] para pagamento em BANCO"')
        @ lci+9,cci+35 GET mcod_ven PICT "999" VALID ven(mcod_ven,lci+10,cci+39) .AND. lim_get() WHEN men_get(lci+10,cci+31,'Informe o Cod. do vendedor "CAMPO NAO OBRIGATORIO"')
        @ lci+10,cci+35 GET mnum_ped PICT "999999" VALID lim_get() WHEN men_get(lci+11,cci+31,'Informe o No.do pedido que corresponde a este documento "CAMPO NAO OBRIGATORIO"')
        @ lci+11,cci+35 GET mqtd_cop PICT "99" VALID lim_get() WHEN men_get(lci+12,cci+31,'Informe a Quantidade de Copias que deseja imprimir')
        READ
        IF LASTKEY()=27
                RETURN NIL
        ENDIF
        IF ! imp_arq('DUPLICATA','D')
                LOOP
        ENDIF

        IF mcgc_firm = '16.314.569/0001-61'
                imprt(mtipo_imp,'C')
                @ PROW()+10,21 SAY mduplicata
                DEVPOS(PROW(),40);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                @ PROW(),60 SAY mduplicata
                DEVPOS(PROW(),80);DEVOUT(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))
                @ PROW(),99 SAY mvenc
                @ PROW()+3,65 SAY STRZERO(mvenc-mdata_sis,3)+' dias'
                @ PROW()+3,60 SAY STRZERO(mfornec,5)
                @ PROW(),PCOL()+1 SAY mnome_cli
                @ PROW()+1,60 SAY mend
                @ PROW()+1,60 SAY RTRIM(mbairro)+'-Cidade: '+RTRIM(mcidade)
                @ PROW(),110 SAY muf
                @ PROW()+1,60 SAY 'Cobranca Bancaria'
                IF mcgc <> SPACE(14)
                        DEVPOS(PROW()+1,60);DEVOUTPICT(mcgc,'@@R 99.999.999/9999-99')
                        DEVPOS(PROW(),100);DEVOUT(minsc)
                ELSEIF mcpf <> SPACE(11)
                        DEVPOS(PROW()+1,60);DEVOUTPICT(mcpf,'@@R 999.999.999-99')
                ELSE
                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                ENDIF
                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2),10))+')'
                mlinha := MLCOUNT(mextenso,50)
                DEVPOS(PROW()+1,00);DEVOUT(' ')
                FOR lin = 1 TO mlinha
                        mlin := MEMOLINE(mextenso,50,lin,,10)
                        DEVPOS(PROW()+1,60);DEVOUT(PADR(RTRIM(mlin),50))
                NEXT
                imprt(mtipo_imp,'N')
        ELSEIF ver_serie() = '141236' // SOTINTAS
                dup_sot(mdata_sis,STRZERO(mcod_ven,3),mvalor,mduplicata,mvenc,mnome_cli,mfornec,mend,;
                        mbairro,mcidade,muf,cli->cep,mcgc,minsc,mcpf,mbanco)
        ELSEIF ver_serie() = '141258' // IND.SAMPAIO
                dup_sam(mdata_sis,STRZERO(mcod_ven,3),mvalor,mduplicata,mvenc,mnome_cli,mfornec,mend,;
                        mbairro,mcidade,muf,cli->cep,mcgc,minsc,mcpf,mbanco)
        ELSE
                FOR i = 1 TO mqtd_cop
                        IF cfg->tipfrmped = 'F'
                                imprt(mtipo_imp,'N')
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
                                @ PROW(),PCOL()+1 SAY STRZERO(mfornec,5)
                                @ PROW(),PCOL()+1 SAY mnome_cli
                                @ PROW()+1,00 SAY 'End.: '+mend
                                @ PROW()+1,00 SAY 'Bairro: '+RTRIM(mbairro)+'-Cidade: '+RTRIM(mcidade)+'-'+muf+'- CEP:'+cli->cep
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
                                @ PROW()+1,00 SAY 'Fatura No.:_______________'
                                @ PROW(),49 SAY 'Cod.:'
                                @ PROW(),55 SAY STRZERO(mcod_ven,3)
                                @ PROW()+1,00 SAY mtraco
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
                                DEVPOS(PROW(),00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(mvalor,'99,999,999.99'))+')',60))
*                               mextenso:= '('+extenso(mvalor,.T.)+')'
                                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2),10))+')'
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
                                EJECT
                        ELSE
                                DEVPOS(PROW(),00);DEVOUT(CHR(18))
                                DEVPOS(PROW(),00);DEVOUT(memp_resa)
                                DEVPOS(PROW()+1,00);DEVOUT(CHR(15))
                                DEVPOS(PROW(),00);DEVOUT(mend_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Fone: '+mfone_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Inscr. '+minsc_firm+'   C.G.C.: '+mcgc_firm)
                                @ PROW()+1,00 SAY mtraco
                                @ PROW()+1,00 SAY ' '
                                @ PROW(),00 SAY 'Cliente:'
                                @ PROW(),09 SAY cli->cod_cli
                                @ PROW(),15 SAY cli->razao
                                @ PROW()+1,00 SAY 'End.: '+cli->endereco
                                @ PROW()+1,00 SAY 'Bairro: '+RTRIM(cli->bairro)+'-Cidade: '+RTRIM(cli->cidade)+'-'+cli->uf+'- CEP:'+cli->cep
                                @ PROW()+1,00 SAY 'Fone: '+cli->tel1
                                IF cli->cgc <> SPACE(14)
                                        @ PROW()+1,00 SAY 'CGC:'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cgc,'@@R 99.999.999/9999-99')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Insc.: '+cli->insc)
                                ELSEIF cli->cpf <> SPACE(11)
                                        @ PROW()+1,00 SAY 'CPF:'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(cli->cpf,'@@R 999.999.999-99')
                                ENDIF
                                @ PROW()+1,00 SAY mtraco
                                @ PROW()+1,00 SAY CHR(18)
                                @ PROW(),00 SAY 'DUPLICATA No.: '+mtipo+mduplicata
                                @ PROW()+1,00 SAY CHR(15)
                                @ PROW(),00 SAY 'Emissao:'
                                @ PROW(),10 SAY memissao
                                @ PROW(),25 SAY 'Vencimento:'
                                @ PROW(),37 SAY mvenc
                                @ PROW(),49 SAY 'Cod.:'
                                @ PROW(),55 SAY STRZERO(mcod_ven,3)
                                @ PROW()+1,00 SAY mtraco
                                @ PROW()+1,00 SAY 'Fatura No.:               '
                                @ PROW(),30 SAY 'No.Duplicata:'
                                @ PROW()+1,00 SAY mtraco
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Valor R$: ('+LTRIM(TRANSFORM(ROUND(mvalor,2),'99,999,999.99'))+')',60))
                                mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2),10))+')'
                                mlinha := MLCOUNT(mextenso,60)
                                FOR lin = 1 TO mlinha
                                        mlin := MEMOLINE(mextenso,60,lin,,10)
                                        DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),60))
                                NEXT
                                @ PROW()+1,00 SAY mtraco
                                mextenso:= 'Reconheco(cemos) a exatidao desta DUPLICATA DE VENDA MERCANTIL, na importancia acima que pagarei(emos) a '+memp_resa+', ou a sua ordem na praca e vencimento indicados.'
                                mlinha := 0
                                mlinha := MLCOUNT(mextenso,60)
                                lin := 0
                                FOR lin = 1 TO mlinha
                                        mlin := MEMOLINE(mextenso,60,lin,,10)
                                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(mlin))
                                NEXT
                                @ PROW()+1,00 SAY mtraco
                                DEVPOS(PROW()+3,00);DEVOUT(PADC('---------------------------------------------',60))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Assinatura do Emitente',60))
                                DEVPOS(PROW()+2,00);DEVOUT(PADC('Em         /        /        ',60))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Data do Aceite',60))
                                DEVPOS(PROW()+3,00);DEVOUT(PADC('---------------------------------------------',60))
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Assinatura do Sacado',60))
                                @ PROW()+1,00 SAY mtraco
                                IF ! EMPTY(set->roda_dup1)
                                        @ PROW()+1,00 SAY PADC(RTRIM(set->roda_dup1),80)
                                        @ PROW()+1,00 SAY PADC(RTRIM(set->roda_dup2),80)
                                        @ PROW()+1,00 SAY mtraco
                                ENDIF
                                DEVPOS(PROW()+cfg->qtdlinped,00);DEVOUT(CHR(18))
                        ENDIF
                NEXT
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
*********************** f i m *************************************




