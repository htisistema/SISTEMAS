MEMVAR getlist,mdata_sis,cod_operado

**************************************
* MENU DOS BOLETOS
**************************************

FUNCTION safbole1(mcli,mdoc,mvlr,mtp,mt)
*****************
LOCAL mprg:='SACBOLE1'
LOCAL tela,lci,cci,lba,cba,mtipo_end,mflag_imp:=' ',mfound,i:=0,meject:=' '

PRIVATE local,mdata,mvenc,n_documento,especie,aceite,mvalor,obs1,;
        obs2,obs3,obs4,obs5,mcod_cli,mendereco,mcliente,mreg:=0,mtipo,mtip,;
        mvalor_ped:=0,mparc:=0,macrecimo:=0

PRIVATE mcpf,mcgc,mnum_ped:=0
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)

lci := 06
cci := 00
lba := 20
cba := 79
tela := SAVESCREEN(01,00,24,79)
mtipo := mtp
mtip  := mt
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
IF mdoc = NIL
        *************
        IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
        IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
        IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
        n_documento := SPACE(10)
        mvalor := 0
        mcod_cli := 0
ELSE
        n_documento := mdoc
        mvalor := mvlr
        mcod_cli := mcli

ENDIF
set_key('cliente')
SELE('cli');ORDSETFOCUS(2)
GO TOP
*************
SET KEY -6 TO f7_cli
mtipo_end := 'P'
local := set->local_bol
*local := 'Ate o vencimento pagavel em qualquer agencia'+SPACE((46-LEN('Ate o vencimento pagavel em qualquer agencia')))
mdata := mdata_sis
mvenc := CTOD('  /  /  ')
IF mtipo = 'DUPLICATA' .OR. mtipo = 'DUPLICATA JW'
        especie := 'C'
ELSE
        especie := 'DUPL'
ENDIF
aceite := SPACE(1)
mcliente := SPACE(40)
mendereco := SPACE(70)
mcgc := SPACE(18)
macrecimo := 0
exibi_prg(mprg)
WHILE .T.
        obs1 := set->mens_bol1
        obs2 := set->mens_bol2
        obs3 := set->mens_bol3
        obs4 := set->mens_bol4
        obs5 := set->mens_bol5
        *************
        SELE('cli');ORDSETFOCUS(2)
        GO TOP
        *************
        mtipo_imp := cfg->imp_bol
        mfound:=' '
        mensagem('Preencha os campos que desejar - <ESC> p/Abandonar')
        botao(lci,cci,lba,cba,,' Emissao de Boleto Bancario < '+mtipo+' >')
        DEVPOS(lci+1,cci+1);DEVOUT('No.Pedido..:')
        DEVPOS(lci+2,cci+1);DEVOUT('Cod.Cliente:')
        DEVPOS(lci+3,cci+1);DEVOUT('CGC/CPF:')
        DEVPOS(lci+4,cci+1);DEVOUT('Tipo Endereco [P]rincipal [C]obranca:')
        DEVPOS(lci+5,cci+1);DEVOUT('Local Pag.:')
        DEVPOS(lci+6,cci+1);DEVOUT('Data:')
        DEVPOS(lci+6,cci+17);DEVOUT('Vencimento:')
        DEVPOS(lci+7,cci+1);DEVOUT('No.Documento:')
        DEVPOS(lci+7,cci+27);DEVOUT('Especie:')
        DEVPOS(lci+7,cci+42);DEVOUT('Aceite:')
        DEVPOS(lci+7,cci+52);DEVOUT('Valor:')
        DEVPOS(lci+8,cci+1);DEVOUT('Outros Acrescimos:')
        DEVPOS(lci+9,cci+1);DEVOUT('Observacao:')
        @ lci+1,cci+14 GET mnum_ped PICT '999999'
        READ
        IF LASTKEY() = 27
                IF ! EMPTY(mflag_imp)
                        SET DEVI TO PRINT
                        SET PRINT TO (cfg->prt_bol)
                        EJECT
                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                ENDIF
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        mcod_cli := 0
        IF ! EMPTY(mnum_ped)
                *************
                SELE('ped_s');ORDSETFOCUS(1)
                GO TOP
                *************
                IF ! ped_s->(DBSEEK(STRZERO(mnum_ped,6)))
                        atencao('Nao foi encontrado este PEDIDO')
                        LOOP
                ENDIF
                IF ! EMPTY(ped_s->pboleto)
                        atencao('Este Pedido ja foi emitido *** BOLETO ***')
                ENDIF
                *************
                SELE('dupr');ORDSETFOCUS(7)
                GO TOP
                *************
                IF dupr->(DBSEEK(STRZERO(mnum_ped,6)))
                        mfound := '*'
                        mreg := RECNO()
                        mcod_cli := VAL(dupr->fornec)
                        mcliente := dupr->cliente
                        ver_cli(mcod_cli,lci+2,cci+20)
                        mdata := dupr->emissao
                        mvenc := dupr->venc
                        mvalor := dupr->valor
                        n_documento := dupr->duplicata
                ELSE
                        *************
                        SELE('ped_s');ORDSETFOCUS(1)
                        GO TOP
                        *************
                        IF ! ped_s->(DBSEEK(STRZERO(mnum_ped,6)))
                                atencao('Nao foi encontrado este PEDIDO')
                                LOOP
                        ENDIF
                        IF ! EMPTY(ped_s->pboleto)
                                atencao('Este Pedido ja foi emitido *** BOLETO ***')
                        ENDIF
                        mreg := RECNO()
                        mcod_cli := VAL(ped_s->pcod_cli)
                        ver_cli(mcod_cli,lci+2,cci+20)
                        mdata := ped_s->pdat_ped
                        mvenc := ped_s->pdat_ped + VAL(SUBSTR(ped_s->pcond_inte,3,3))
                        mparc := VAL(SUBSTR(ped_s->pcond_veze,1,1))+VAL(SUBSTR(ped_s->pcond_veze,2))
                        n_documento := ped_s->pnum_ped+'/01'
                        mvalor_ped := 0
                        WHILE STRZERO(mnum_ped,6) = ped_s->pnum_ped
                                mvalor_ped := mvalor_ped + (ped_s->pquantd * ped_s->pvlr_fat)
                                SKIP
                        ENDDO
                        mvalor := mvalor_ped / mparc
                ENDIF
        ENDIF
        @ lci+2,cci+14 GET mcod_cli PICT '99999' VALID ver_cli(mcod_cli,lci+2,cci+20)
        @ lci+2,cci+20 GET mcliente PICT '@!' WHEN EMPTY(mcod_cli)
        @ lci+3,cci+10 GET mcgc
        @ lci+4,cci+39 GET mtipo_end PICT '@!' VALID mtipo_end $ 'P,C'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF ! EMPTY(mcod_cli)
                mcliente := cli->razao
                IF mtipo_end = 'P'
                        mendereco := RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep
                ELSE
                        mendereco := RTRIM(cli->end_cob)+'-'+RTRIM(cli->bairro_cob)+'-'+RTRIM(cli->cidade_cob)+'-'+RTRIM(cli->uf_cob)+'- CEP.:'+cli->cep_cob
                ENDIF
/*
                setcor(3)
                IF ! EMPTY(mcpf)
                        DEVPOS(lci+3,cci+10);DEVOUT(mcpf)
                ELSE
                        DEVPOS(lci+3,cci+10);DEVOUT(mcgc)
                ENDIF
                setcor(1)
*/
        ENDIF
        limpa(lci+4,cci+1,lci+4,cba-1)
        DEVPOS(lci+4,cci+1);DEVOUT('End.:')
        @ lci+4,cci+7 GET mendereco PICT '@S70!'
        @ lci+5,cci+13 GET local
        @ lci+6,cci+7 GET mdata
        @ lci+6,cci+29 GET mvenc VALID IF(mvenc<mdata,.F.,.T.)
        @ lci+7,cci+15 GET n_documento PICT '@!' VALID IF(EMPTY(n_documento),.F.,.T.)
        @ lci+7,cci+36 GET especie PICT '@!'
        @ lci+7,cci+50 GET aceite PICT '@!'
        @ lci+7,cci+59 GET mvalor PICT '999,999.99'
        @ lci+8,cci+20 GET macrecimo PICT '999,999.99'
        READ
        obs1 := STRTRAN(obs1,'@$juros',ALLTRIM(TRANSFORM((set->juros_dup/100)*mvalor,'999,999.99')))
        obs2 := STRTRAN(obs2,'@$juros',ALLTRIM(TRANSFORM((set->juros_dup/100)*mvalor,'999,999.99')))
        obs3 := STRTRAN(obs3,'@$juros',ALLTRIM(TRANSFORM((set->juros_dup/100)*mvalor,'999,999.99')))
        obs4 := STRTRAN(obs4,'@$juros',ALLTRIM(TRANSFORM((set->juros_dup/100)*mvalor,'999,999.99')))
        obs5 := STRTRAN(obs5,'@$juros',ALLTRIM(TRANSFORM((set->juros_dup/100)*mvalor,'999,999.99')))

        obs1 := STRTRAN(obs1,'@%juros',ALLTRIM(TRANSFORM(set->juros_dup,'999.99')))
        obs2 := STRTRAN(obs2,'@%juros',ALLTRIM(TRANSFORM(set->juros_dup,'999.99')))
        obs3 := STRTRAN(obs3,'@%juros',ALLTRIM(TRANSFORM(set->juros_dup,'999.99')))
        obs4 := STRTRAN(obs4,'@%juros',ALLTRIM(TRANSFORM(set->juros_dup,'999.99')))
        obs5 := STRTRAN(obs5,'@%juros',ALLTRIM(TRANSFORM(set->juros_dup,'999.99')))

        IF mtipo <> 'DUPLICATA' .AND. mtipo <> 'DUPLICATA JW'
                @ lci+9,cci+13 GET obs1
                @ lci+10,cci+13 GET obs2
                @ lci+11,cci+13 GET obs3
                @ lci+12,cci+13 GET obs4
                @ lci+13,cci+13 GET obs5
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
        meject := mensagem1('Dar AVANCO DE PAGINA apos a impressao do boleto:','N','S,N')
        IF LASTKEY() = 27;LOOP;ENDIF
        IF ! imp_arq('BOLETO.REL','B')
                LOOP
        ENDIF
        IF EMPTY(mnum_ped)
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
                        IF meject = 'S';EJECT;ENDIF
                ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
                        mflag_imp := '*'
                        bol_brades()
                ELSEIF mtipo == 'DUPLICATA'
                        dup_sam(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                        IF meject = 'S';EJECT;ENDIF
                ELSEIF mtipo == 'DUPLICATA JW'
                        dup_jw(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                        IF meject = 'S';EJECT;ENDIF
                ENDIF
        ELSE
                IF mfound = '*'
                        *************
                        SELE('dupr');ORDSETFOCUS(7)
                        GO mreg
                        *************
                        WHILE STRZERO(mnum_ped,6) = dupr->num_ped .AND. ! EOF()
                                IF ! EMPTY(dupr->datpag)
                                        SKIP
                                        LOOP
                                ENDIF
                                mvenc := dupr->venc
                                mdata := dupr->emissao
                                n_documento := dupr->duplicata
                                mvalor := dupr->valor
                                IF mtipo = 'ITAU' .OR. mtipo = 'BILBAO'
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
                                        IF meject = 'S';EJECT;ENDIF
                                ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
                                        bol_brades()
                                ELSEIF mtipo == 'DUPLICATA'
                                        dup_sam(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                                        IF meject = 'S';EJECT;ENDIF
                                ELSEIF mtipo == 'DUPLICATA JW'
                                        dup_jw(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                                        IF meject = 'S';EJECT;ENDIF
                                ENDIF
                                SKIP
                        ENDDO
                ELSE
                        *************
                        SELE('ped_s');ORDSETFOCUS(1)
                        GO mreg
                        *************
                        i := 1
                        FOR i = 1 TO mparc
                                mvenc := ped_s->pdat_ped + VAL(SUBSTR(ped_s->pcond_inte,3*i,3))
                                mdata := ped_s->pdat_ped
                                n_documento := ALLTRIM(STR(VAL(ped_s->pnum_ped)))+'/'+STRZERO(i,2)
                                mvalor := mvalor_ped / mparc
                                IF mtipo = 'ITAU' .OR. mtipo = 'BILBAO'
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
                                        IF meject = 'S';EJECT;ENDIF
                                ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
                                        bol_brades()
                                ELSEIF mtipo == 'DUPLICATA'
                                        dup_sam(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                                        IF meject = 'S';EJECT;ENDIF
                                ELSEIF mtipo == 'DUPLICATA JW'
                                        dup_jw(mdata,cod_operado,mvalor,n_documento,mvenc,mcliente,mcod_cli,cli->endereco,;
                                                cli->bairro,cli->cidade,cli->uf,cli->cep,cli->cgc,cli->insc,cli->cpf,especie)
                                        IF meject = 'S';EJECT;ENDIF
                                ENDIF
                        NEXT
                ENDIF
                *************
                SELE('ped_s');ORDSETFOCUS(1)
                GO TOP
                *************
                ped_s->(DBSEEK(STRZERO(mnum_ped,6)))
                WHILE ped_s->pnum_ped = STRZERO(mnum_ped,6) .AND. ! EOF()
                        BLOQREG()
                        ped_s->pboleto := '*'
                        SKIP
                ENDDO
                DBCOMMITALL()
                DBUNLOCKALL()
        ENDIF
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('BOLETO.REL')
        ENDIF
ENDDO
RETURN NIL
****************************** fim **********************************
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
********************************* F I M ********************************
********************************
* BOLETO BANCO BANDEPE
*******************************
FUNCTION bol_bandep

MEMVAR mtipo_imp,mtipo,local,mvenc,mdata,n_documento,mvalor,;
       obs1,obs2,obs3,obs4,obs5,mcgc,mcpf

imprt(mtipo_imp,'P8')
imprt(mtipo_imp,'N',1)
DEVPOS(PROW(),00);DEVOUT(local)
IF mtipo = 'HSBC'
        DEVPOS(PROW(),61);DEVOUT(mvenc)
ELSE
        DEVPOS(PROW(),50);DEVOUT(mvenc)
ENDIF
IF set->ceden_bol = 'S'
        DEVPOS(PROW()+3,00);DEVOUT(set->razao)
ELSE
        DEVPOS(PROW()+3,00);DEVOUT(' ')
ENDIF
DEVPOS(PROW()+2,00);DEVOUT(mdata)
DEVPOS(PROW(),11);DEVOUT(n_documento)
IF mtipo = 'HSBC'
        DEVPOS(PROW()+2,55);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
ELSE
        DEVPOS(PROW()+2,46);DEVOUT('R$ '+TRANSFORM(mvalor,'999,999.99'))
ENDIF
DEVPOS(PROW()+3,00);DEVOUT(obs1)
DEVPOS(PROW()+1,00);DEVOUT(obs2)
DEVPOS(PROW()+1,00);DEVOUT(obs3)
DEVPOS(PROW()+1,00);DEVOUT(obs4)
DEVPOS(PROW()+1,00);DEVOUT(obs5)
DEVPOS(PROW()+5,00);DEVOUT(cli->cod_cli+'-'+LEFT(cli->razao,37))
imprt(mtipo_imp,'C',1)
DEVPOS(PROW(),00);DEVOUT(RTRIM(cli->endereco)+'-'+RTRIM(cli->bairro)+'-'+RTRIM(cli->cidade)+'-'+RTRIM(cli->uf)+'- CEP.:'+cli->cep)
IF ! EMPTY(cli->cpf)
        DEVPOS(PROW(),PCOL()+5);DEVOUT('CPF: '+TRANSFORM(mcpf,'@@R 999.999.999-99'))
ELSE
        DEVPOS(PROW(),PCOL()+5);DEVOUT('CGC: '+TRANSFORM(mcgc,'@@R 99.999.999/9999-99'))
ENDIF
imprt(mtipo_imp,'N',11)
SETPRC(00,00)
imprt(mtipo_imp,'P6')
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

