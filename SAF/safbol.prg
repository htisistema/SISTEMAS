*********************************************
* FUNCAO P/IMPRIMIR BOLETOS BANCARIOS
*********************************************

FUNCTION safbole1(mtip)
*****************
LOCAL mprg:='SAFBOLE1'
LOCAL tela,opcao,lci,cci,lba,cba,i:=0,mfaixa
PRIVATE mparcela,mparcela1,msele,morde,mvalor,mvlr_bolsa:=0,mvenc,mdata,;
        mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35),mnum_conta:=SPACE(16),mmatricula:=SPACE(6),;
        mgstt:=SPACE(4),n_documento,mresponsa,mcpf,mend,mobs1,mobs2,mobs3,mobs4,;
        mobs5,mflag_imp:=' ',mtipo:=mtip,mtipo_doc:='ME'

IF ! ver_nivel(mprg,'EMISSAO DE BOLETO BANCARIOS','125')
        RETURN NIL
ENDIF

lci := 07
cci := 03
lba := 18
cba := 79
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('nobrega','nobre');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('nobre')
**************
mdata      := mdata_sis
mmatricula := SPACE(6)
mgstt      := SPACE(4)
mparcela  := SPACE(2)
mparcela1 := SPACE(2)
mobs1      := '- Ate o vencimento receber o valor R$:'
mobs2      := '- Ate 30 dias apos vencimento receber R$:'
mobs3      := '  (2% multa)'
mobs4      := '- Apos 30 dias do vencimento receber o valor do'
mobs5      := '  documento mais multa de 2% ao mes e juros de '
mobs6      := '  1% ao mes'
mobs7      := '- Apos 60 dias sera encaminhado para protesto'
@ 23,00 CLEAR TO 23,79
setcor(3)
botao(lci,cci,lba,cba)
setcor(1)
DEVPOS(lci+1,cci+1);DEVOUT('Tipo de Documento..................:')
DEVPOS(lci+2,cci+1);DEVOUT('Matricula do Aluno que deseja......:')
DEVPOS(lci+3,cci+1);DEVOUT('Digite o GSTT que deseja...........:')
DEVPOS(lci+4,cci+1);DEVOUT('Digite Parcela que deseja [01...12]:    a ')
DEVPOS(lci+5,cci+1);DEVOUT('Instrucoes:')
@ lci+1,cci+38 GET mtipo_doc PICT '@!'
@ lci+2,cci+38 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.T.,ver_aluno(mmatricula,,lci+2,cci+46))
@ lci+3,cci+38 GET mgstt PICT '@!' VALID IF(EMPTY(mgstt),.T.,ver_gstt(mgstt)) WHEN EMPTY(mmatricula)
@ lci+4,cci+38 GET mparcela PICT '99' VALID mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12'
@ lci+4,cci+44 GET mparcela1 PICT '99' VALID IF(VAL(mparcela)>VAL(mparcela1),.F.,mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12')
@ lci+5,cci+44 GET mobs1
@ lci+6,cci+44 GET mobs2
@ lci+7,cci+44 GET mobs3
@ lci+8,cci+44 GET mobs4
@ lci+9,cci+44 GET mobs5
@ lci+10,cci+44 GET mobs6
@ lci+11,cci+44 GET mobs7
READ
IF LASTKEY() = 27
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
opcao := ' '
opcao := mensagem1('Confirma a Operacao:','S','S,N')
IF LASTKEY() = 27 .OR. opcao = 'N'
        RETURN NIL
ENDIF
IF mtipo_doc = 'ME'
        ********************************
        SELE('aluno');ORDSETFOCUS(6)
        GO TOP
        ********************************
        msele := SELE()
        morde := INDEXORD()
        mseq := 1
        mlin := SAVESCREEN(24,00,24,79)
        i:= 0
        *i := VAL(mparcela)
        IF ! imp_arq('BOLETO.REL')
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        WHILE ! EOF()
                IF (aluno->gstt = 'DEBI');
                   .OR. (! EMPTY(mgstt) .AND. mgstt <> aluno->gstt);
                   .OR. (! EMPTY(mmatricula) .AND. mmatricula <> aluno->matricula)
                        SKIP
                        LOOP
                ENDIF
                i:= 0
                FOR i = VAL(mparcela) TO VAL(mparcela1)         //-VAL(mparcela)
                        IF STRZERO(i,2) = '01'
                                mfaixa := aluno->faixa_1
                        ELSEIF STRZERO(i,2) = '02'
                                mfaixa := aluno->faixa_2
                        ELSEIF STRZERO(i,2) = '03'
                                mfaixa := aluno->faixa_3
                        ELSEIF STRZERO(i,2) = '04'
                                mfaixa := aluno->faixa_4
                        ELSEIF STRZERO(i,2) = '05'
                                mfaixa := aluno->faixa_5
                        ELSEIF STRZERO(i,2) = '06'
                                mfaixa := aluno->faixa_6
                        ELSEIF STRZERO(i,2) = '07'
                                mfaixa := aluno->faixa_7
                        ELSEIF STRZERO(i,2) = '08'
                                mfaixa := aluno->faixa_8
                        ELSEIF STRZERO(i,2) = '09'
                                mfaixa := aluno->faixa_9
                        ELSEIF STRZERO(i,2) = '10'
                                mfaixa := aluno->faixa_10
                        ELSEIF STRZERO(i,2) = '11'
                                mfaixa := aluno->faixa_11
                        ELSEIF STRZERO(i,2) = '12'
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
        *                       atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
        *                       SKIP
                                LOOP
                        ENDIF
                        mvalor := 0
                        mvlr_bolsa := 0
                        IF ver_fx(mfaixa)
                                mvlr_bolsa := tabmen->valor - (tabmen->valor*(fx->desconto/100))
                        ENDIF
                        mvalor := tabmen->valor
                        IF fx->desconto = 100 .OR. mfaixa = 'S'
                                LOOP
                        ENDIF
                        IF ! matparc(aluno->matricula,STRZERO(i,2))
                                LOOP
                        ENDIF
                        SETPOS(24,00);DISPOUT('Esta Gerando o carne em arquivo, aguarde processamento MAT: '+aluno->matricula)
                        mobs1      := mobs1+TRANSFORM(mvlr_bolsa,'999.99')
                        mobs2      := mobs2+TRANSFORM(mvlr_bolsa*1.02,'999.99')
                        mvenc      := CTOD(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3))
                        n_documento:= aluno->matricula+STRZERO(i,2)
                        mgstt      := aluno->gstt
                        mmatricula := aluno->matricula
                        mresponsa  := aluno->responsa
                        mcpf       := aluno->r_cpf
                        mend       := RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+'-'+RTRIM(aluno->r_apto)+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado
                        IF mtipo = 'ITAU' .OR. mtipo = 'BILBAO'
                                mflag_imp := '*'
                                bol_itau()
                        ELSEIF mtipo = 'UNIBANCO'
                                bol_unib()
                                IF meject = 'S';EJECT;ENDIF
                        ELSEIF mtipo = 'RURAL'
                                bol_rural()
                                IF meject = 'S';EJECT;ENDIF
                        ELSEIF mtipo = 'REAL'
                                bol_real()
                                IF meject = 'S';EJECT;ENDIF
                        ELSEIF mtipo = 'BANDEPE' .OR. mtipo = 'HSBC'
                                bol_bandep()
                        ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
                                mflag_imp := '*'
                                bol_brades()
                        ENDIF
                NEXT
                SELE(msele);ORDSETFOCUS(morde)
                SKIP
        ENDDO
ELSE
        ********************************
        SELE('aluno');ORDSETFOCUS(11)
        GO TOP
        ********************************
        msele := SELE()
        morde := INDEXORD()
        mseq := 1
        mlin := SAVESCREEN(24,00,24,79)
        i:= 0
        *i := VAL(mparcela)
        IF ! imp_arq('BOLETO.REL')
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        WHILE ! EOF()
                IF (mtipo_doc <> cdmen->tipo);
                   .OR. (! EMPTY(mmatricula) .AND. mmatricula <> cdmen->matricula);
                   .OR. (! EMPTY(mgstt) .AND. mgstt <> cdmen->gstt)
                        SKIP
                        LOOP
                ENDIF
                IF VAL(cdmen->parcela) < VAL(mparcela) .OR. VAL(cdmen->parcela) > VAL(mparcela1)
                        SKIP
                        LOOP
                ENDIF
                ver_aluno(cdmen->matricula,,,)
                mvalor := 0
                mvlr_bolsa := 0
                mvalor := cdmen->vlr_parc
                SETPOS(24,00);DISPOUT('Esta Gerando o carne em arquivo, aguarde processamento MAT: '+cdmen->matricula)
                mobs1      := mobs1+TRANSFORM(mvalor,'999.99')
                mobs2      := mobs2+TRANSFORM(mvalor*1.02,'999.99')
                mvenc      := cdmen->data_venc
                n_documento:= cdmen->matricula+cdmen->parcela
                mgstt      := cdmen->gstt
                mmatricula := cdmen->matricula
                mresponsa  := aluno->responsa
                mcpf       := aluno->r_cpf
                mend       := RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+'-'+RTRIM(aluno->r_apto)+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado
                IF mtipo = 'ITAU' .OR. mtipo = 'BILBAO'
                        mflag_imp := '*'
                        bol_itau()
                ELSEIF mtipo = 'UNIBANCO'
                        bol_unib()
                        IF meject = 'S';EJECT;ENDIF
                ELSEIF mtipo = 'RURAL'
                        bol_rural()
                        IF meject = 'S';EJECT;ENDIF
                ELSEIF mtipo = 'REAL'
                        bol_real()
                        IF meject = 'S';EJECT;ENDIF
                ELSEIF mtipo = 'BANDEPE' .OR. mtipo = 'HSBC'
                        bol_bandep()
                ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
                        mflag_imp := '*'
                        bol_brades()
                ENDIF
                SELE(msele);ORDSETFOCUS(morde)
                SKIP
        ENDDO
ENDIF
SETPRC(00,00)
EJECT
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
RESTSCREEN(24,00,24,79,mlin)
atencao('Processamento concluido com sucesso',3)
IF mimp_tipo = 2
        lertexto('BOLETO.REL')
ENDIF
SET CENTURY OFF
RETURN NIL
********************************* F I M ********************************
********************************
* BOLETO BANCO BANDEPE
*******************************
FUNCTION bol_bandep

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N',1)
DEVPOS(PROW(),00);DEVOUT(' ')
IF mtipo = 'HSBC'
        DEVPOS(PROW(),61);DEVOUT(mvenc)
ELSE
        DEVPOS(PROW(),50);DEVOUT(mvenc)
ENDIF
DEVPOS(PROW()+3,00);DEVOUT(' ')
DEVPOS(PROW()+2,00);DEVOUT(mdata)
DEVPOS(PROW(),11);DEVOUT(n_documento)
IF mtipo = 'HSBC'
        DEVPOS(PROW()+2,55);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
ELSE
        DEVPOS(PROW()+2,50);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
ENDIF
DEVPOS(PROW()+3,00);DEVOUT(mobs1)
DEVPOS(PROW()+1,00);DEVOUT(mobs2)
DEVPOS(PROW()+1,00);DEVOUT(mobs3)
DEVPOS(PROW()+1,00);DEVOUT(mobs4)
DEVPOS(PROW()+1,00);DEVOUT(mobs5)
DEVPOS(PROW()+1,00);DEVOUT(mobs6)
DEVPOS(PROW()+1,00);DEVOUT(mobs7)
DEVPOS(PROW()+3,00);DEVOUT(mresponsa)
imprt(mtipo_imp,'C')
DEVPOS(PROW()+1,00);DEVOUT(mend)
DEVPOS(PROW(),PCOL()+5);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
imprt(mtipo_imp,'N',11)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
**************************** FIM ******************************************
**************************************************
* BOLETO BANCO ITAU
**************************************************
FUNCTION bol_itau()

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,especie,aceite,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mcod_cli,mcliente,mcgc,mcpf,mendereco

imprt(mtipo_imp,'P8')
IF mtipo = 'ITAU'
        imprt(mtipo_imp,'N',1)
        DEVPOS(PROW()+2,00);DEVOUT(local)
ELSEIF mtipo = 'BILBAO'
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT(local)
ENDIF
DEVPOS(PROW(),50);DEVOUT(mvenc)
DEVPOS(PROW()+4,00);DEVOUT(mdata)
DEVPOS(PROW(),16);DEVOUT(n_documento)
DEVPOS(PROW(),30);DEVOUT(especie)
DEVPOS(PROW(),38);DEVOUT(aceite)
DEVPOS(PROW()+3,50);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
DEVPOS(PROW()+3,00);DEVOUT(obs1)
DEVPOS(PROW()+1,00);DEVOUT(obs2)
DEVPOS(PROW()+1,00);DEVOUT(obs3)
DEVPOS(PROW()+1,00);DEVOUT(obs4)
DEVPOS(PROW()+1,00);DEVOUT(obs5)
DEVPOS(PROW()+5,00);DEVOUT(STRZERO(mcod_cli,5)+' - '+mcliente)
IF ! EMPTY(mcod_cli)
        IF ! EMPTY(mcpf)
                DEVPOS(PROW()+1,00);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
        ENDIF
ELSE
        DEVPOS(PROW()+1,00);DEVOUT(mcgc)
ENDIF
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),00);DEVOUT(mendereco)
imprt(mtipo_imp,'N',8)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
******************************** F I M *************************************
**************************************************
* BOLETO BANCO UNIBANCO
**************************************************
FUNCTION bol_unib()

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,especie,aceite,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N',1)
DEVPOS(PROW()+2,03);DEVOUT(local)
DEVPOS(PROW(),55);DEVOUT(mvenc)
DEVPOS(PROW()+4,03);DEVOUT(mdata)
DEVPOS(PROW(),21);DEVOUT(n_documento)
DEVPOS(PROW(),35);DEVOUT(especie)
DEVPOS(PROW(),41);DEVOUT(aceite)
DEVPOS(PROW()+2,55);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
DEVPOS(PROW()+3,03);DEVOUT(obs1)
DEVPOS(PROW()+1,03);DEVOUT(obs2)
DEVPOS(PROW()+1,03);DEVOUT(obs3)
DEVPOS(PROW()+1,03);DEVOUT(obs4)
DEVPOS(PROW()+1,03);DEVOUT(obs5)
DEVPOS(PROW()+1,55);DEVOUT('R$ '+TRANSFORM(macrecimo,'999,999.99'))
DEVPOS(PROW()+4,05);DEVOUT(cli->cod_cli+' - '+cli->razao)
IF ! EMPTY(cli->cpf)
        DEVPOS(PROW()+1,05);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
ELSE
        DEVPOS(PROW()+1,05);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
ENDIF
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),10);DEVOUT(RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
imprt(mtipo_imp,'N',7)          //quantidade de linha p/pula
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
***************************** F I M ************************************
**************************************************
* BOLETO BANCO REAL
**************************************************
FUNCTION bol_real

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,especie,aceite,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mtip,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N',1)
DEVPOS(PROW()+1,00);DEVOUT(local)
DEVPOS(PROW(),55);DEVOUT(mvenc)
DEVPOS(PROW()+4,00);DEVOUT(mdata)
DEVPOS(PROW(),10);DEVOUT(n_documento)
DEVPOS(PROW(),35);DEVOUT(especie)
DEVPOS(PROW(),36);DEVOUT(aceite)
DEVPOS(PROW()+2,50);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
DEVPOS(PROW()+3,00);DEVOUT(obs1)
DEVPOS(PROW()+1,00);DEVOUT(obs2)
DEVPOS(PROW()+1,00);DEVOUT(obs3)
DEVPOS(PROW()+1,00);DEVOUT(obs4)
DEVPOS(PROW()+1,00);DEVOUT(obs5)
IF mtip = NIL
        DEVPOS(PROW()+5,00);DEVOUT(cli->cod_cli+' - '+cli->razao)
        IF ! EMPTY(cli->cpf)
                DEVPOS(PROW()+1,00);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
        ENDIF
ELSE
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+5,05);DEVOUT(cli->cod_cli+' - '+cli->razao)
        IF ! EMPTY(mcpf)
                DEVPOS(PROW(),PCOL()+30);DEVOUT(TRANSFORM(mcpf,'@@R 999.999.999-99'))
        ELSE
                DEVPOS(PROW(),PCOL()+30);DEVOUT(TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
        ENDIF
ENDIF
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),00);DEVOUT(RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep)
imprt(mtipo_imp,'P6')
imprt(mtipo_imp,'N',1)
SETPRC(00,00)
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
********************* F I M **************************************
*******************************
* BOLETO BANCO BRADESCO
*******************************
FUNCTION bol_brades

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,mvalor,aceite,;
       obs1,obs2,obs3,obs4,obs5,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N')
DEVPOS(PROW()+2,00);DEVOUT(local)
DEVPOS(PROW(),50);DEVOUT(mvenc)
imprt(mtipo_imp,'C')
DEVPOS(PROW()+4,00);DEVOUT(mdata)
DEVPOS(PROW(),20);DEVOUT(n_documento)
IF mtipo = 'BRADESCO'
        DEVPOS(PROW(),54);DEVOUT(aceite)
ELSEIF mtipo = 'BB'
        DEVPOS(PROW(),64);DEVOUT(aceite)
ENDIF
imprt(mtipo_imp,'N')
DEVPOS(PROW()+2,45);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
DEVPOS(PROW()+4,00);DEVOUT(obs1)
DEVPOS(PROW()+1,00);DEVOUT(obs2)
DEVPOS(PROW()+1,00);DEVOUT(obs3)
DEVPOS(PROW()+1,00);DEVOUT(obs4)
DEVPOS(PROW()+1,00);DEVOUT(obs5)
DEVPOS(PROW()+4,00);DEVOUT(cli->cod_cli+' - '+cli->razao)
IF ! EMPTY(cli->cpf)
        DEVPOS(PROW()+1,00);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
ELSE
        DEVPOS(PROW()+1,00);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
ENDIF
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),00);DEVOUT(RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep)
imprt(mtipo_imp,'N',10)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
**************************************************
* BOLETO BANCO RURAL
**************************************************
FUNCTION bol_rural()

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,especie,aceite,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N')
DEVPOS(PROW()+2,03);DEVOUT(local)
DEVPOS(PROW(),70);DEVOUT(mvenc)
DEVPOS(PROW()+4,00);DEVOUT(mdata)
DEVPOS(PROW(),15);DEVOUT(n_documento)
DEVPOS(PROW(),30);DEVOUT(especie)
DEVPOS(PROW(),41);DEVOUT(aceite)
DEVPOS(PROW()+2,66);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
DEVPOS(PROW()+4,03);DEVOUT(obs1)
DEVPOS(PROW()+1,03);DEVOUT(obs2)
DEVPOS(PROW()+1,03);DEVOUT(obs3)
DEVPOS(PROW()+1,03);DEVOUT(obs4)
DEVPOS(PROW()+1,03);DEVOUT(obs5)
DEVPOS(PROW(),66);DEVOUT('R$ '+TRANSFORM(macrecimo,'999,999.99'))
DEVPOS(PROW()+4,05);DEVOUT(cli->cod_cli+' - '+cli->razao)
IF ! EMPTY(cli->cpf)
        DEVPOS(PROW()+1,05);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
ELSE
        DEVPOS(PROW()+1,05);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
ENDIF
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),10);DEVOUT(RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
imprt(mtipo_imp,'N',7)          //quantidade de linha p/pula
mvenc := CTOD('  /  /  ')
n_documento := SPACE(10)
mvalor := 0
RETURN NIL
***************************** F I M ************************************

