************************************
* GERAR DISCO PARA SEGURADORA
************************************

FUNCTION safdisco
*****************
LOCAL mprg:='SAFDISCO',;
      tela,lci,cci,lba,cba,opcao,mdata1,mdata2,mmes,mtotal,mmulta,mbanco,;
      mcart,mcont,mtraco,mbolsa

lci := 10
cci := 10
lba := 14
cba := 40
tela := SAVESCREEN(01,00,24,79)
opcao := 'S'
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
mtraco := REPLI('-',80)
WHILE .T.
        exibi_prg(mprg)
        mdata1 := CTOD('  /  /  ')
        mdata2 := CTOD('  /  /  ')
        mmes := SPACE(2)
        mtotal := 0
        mmulta := 0
        mcont := 0
        mcart := 0
        mbanco := 0
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Data Inicial:')
        DEVPOS(lci+2,cci+1);DEVOUT('Data Final..:')
        DEVPOS(lci+3,cci+1);DEVOUT('Parcela.....:')
        @ lci+1,cci+15 GET mdata1 VALID IF(EMPTY(mdata1),.F.,.T.)
        @ lci+2,cci+15 GET mdata2 VALID IF(EMPTY(mdata2),.F.,.T.)
        @ lci+3,cci+15 GET mmes VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        opcao := mensagem1('Deseja gerar o Disco p/Seguradora','S','S,N')
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'S'
                mensagem('Gerando disco da seguradora, Aguarde um momento OK')
*               COPY FILE ('\'+mdrive+'\'+mdiretorio+'\CDMENSAL.DBF') TO ('\'+mdrive+'\'+mdiretorio+'\DISCOSEG.DBF')
                COPY FILE ('\'+mdrive+'\CDMENSAL.DBF') TO ('\'+mdrive+'\'+mdiretorio+'\DISCOSEG.DBF')
                IF ! abriarq('discoseg','disco','E');RETURN NIL;ENDIF
*                USE DISCOSEG ALIAS disco EXCLUSIVE NEW
                SELE('disco')
                GO TOP
                BLOQARQ()
                WHILE ! EOF()
                        IF disco->ano <> mdiretorio .OR. disco->gstt = 'DEBI' .OR. disco->tipo <> 'ME'
                                DELE
                                SKIP
                                LOOP
                        ENDIF
/*
                        ver_aluno(disco->matricula,'*')
                        IF mmes = '01'
                                mbolsa := aluno->faixa_1
                        ELSEIF mmes = '02'
                                mbolsa := aluno->faixa_2
                        ELSEIF mmes = '03'
                                mbolsa := aluno->faixa_3
                        ELSEIF mmes = '04'
                                mbolsa := aluno->faixa_4
                        ELSEIF mmes = '05'
                                mbolsa := aluno->faixa_5
                        ELSEIF mmes = '06'
                                mbolsa := aluno->faixa_6
                        ELSEIF mmes = '07'
                                mbolsa := aluno->faixa_7
                        ELSEIF mmes = '08'
                                mbolsa := aluno->faixa_8
                        ELSEIF mmes = '09'
                                mbolsa := aluno->faixa_9
                        ELSEIF mmes = '10'
                                mbolsa := aluno->faixa_10
                        ELSEIF mmes = '11'
                                mbolsa := aluno->faixa_11
                        ELSEIF mmes = '12'
                                mbolsa := aluno->faixa_12
                        ENDIF
*/
*                       IF disco->data_pag > mdata2 .OR. disco->data_pag < mdata1 .OR. disco->parcela <> mmes .OR. disco->valor = 0 .OR. mbolsa = 'S'
                        IF disco->data_pag > mdata2 .OR. disco->data_pag < mdata1 .OR. disco->parcela <> mmes .OR. disco->valor = 0 .OR. disco->bolsa = 'S'
                                DELE
                                SKIP
                                LOOP
                        ENDIF
                        SKIP
                ENDDO
                GO TOP
                PACK
                DBCOMMITALL()
                DBUNLOCKALL()
                CLOSE ALL
        ELSE
                IF ! abriarq('discoseg','disco','E');RETURN NIL;ENDIF
        ENDIF
        IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
        IF ! abriarq('discoseg','disco','E');RETURN NIL;ENDIF
        WHILE ! EOF()
                ver_aluno(disco->matricula,'*')
                disco->aluno := aluno->aluno
                IF VAL(SUBSTR(disco->gstt,1,2)) <= 2
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .57165)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .75)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .70754)
                        ENDIF
                        disco->compl := (disco->valor * .370674)
                        disco->multa := 0
                ELSEIF VAL(SUBSTR(disco->gstt,1,2)) = 5 .OR. VAL(SUBSTR(disco->gstt,1,2)) = 3
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .56094)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .5610)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .56093)
                        ENDIF
                        disco->compl := (disco->valor * .382462)
                        disco->multa := 0
                ELSEIF VAL(SUBSTR(disco->gstt,1,2)) <= 14
* 2003                  disco->valor := (disco->valor * .54082)
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .51506)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .4683)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .44173)
                        ENDIF
                        disco->compl := (disco->valor * .404621)
                        disco->multa := 0
                ELSEIF VAL(SUBSTR(disco->gstt,1,2)) <= 18
*                       disco->valor := (disco->valor * .52465)
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .47695)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .4336)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .40904)
                        ENDIF
                        disco->compl := (disco->valor * .422433)
                        disco->multa := 0
                ELSEIF VAL(SUBSTR(disco->gstt,1,2)) <= 22
*                       disco->valor := (disco->valor * .50705)
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .46096)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .4191)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .39532)
                        ENDIF
                        disco->compl := (disco->valor * .441799)
                        disco->multa := 0
                ELSEIF VAL(SUBSTR(disco->gstt,1,2)) = 23
*                       disco->valor := (disco->valor * .49777)
                        IF mdiretorio ='2004'
                                disco->valor := (disco->valor * .44444)
                        ELSEIF mdiretorio = '2005'
                                disco->valor := (disco->valor * .4041)
                        ELSEIF mdiretorio = '2006'
                                disco->valor := (disco->valor * .38115)
                        ENDIF
                        disco->compl := (disco->valor * .452015)
                        disco->multa := 0
                ENDIF
                SKIP
        ENDDO
        DBCOMMITALL()
        DBUNLOCKALL()
        CLOSE ALL
        mensagem1('Insira o disco no drive A: p/fazer a copia p/o disco OK e pressione <ENTER> p/continuar')
        COPY FILE ('\'+mdrive+'\'+mdiretorio+'\DISCOSEG.DBF') TO A:DISCOSEG.DBF
        atencao('Fim da copia, pressione <ENTER> p/continuar')
        opcao := mensagem1('Deseja Imprimir o conteudo do DISCO','S','S,N')
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'N'
                LOOP
        ENDIF
        IF ! abriarq('discoseg','disco','E');RETURN NIL;ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
*       SET PRINT TO 'SEGURO.REL'
        * SET PRINT TO LPT1
        mpag := 1
        mtit := 'Relacao de Pagamentos p/ Seguradora'
        cabecalho(mpag,mtit)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('GSTT')
        DEVPOS(PROW(),06);DEVOUT('Matricula')
        DEVPOS(PROW(),17);DEVOUT('Parc.')
        DEVPOS(PROW(),24);DEVOUT('Data Pag.')
        DEVPOS(PROW(),38);DEVOUT('Valor')
        DEVPOS(PROW(),48);DEVOUT('Multa')
        DEVPOS(PROW(),55);DEVOUT('Vlr.Pago')
        DEVPOS(PROW(),65);DEVOUT('Tipo Pag.')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        imprt('M','N')
        GO TOP
        WHILE ! EOF()
                DEVPOS(PROW()+1,00);DEVOUT(disco->gstt)
                DEVPOS(PROW(),06);DEVOUT(disco->matricula)
                DEVPOS(PROW(),18);DEVOUT(disco->parcela)
                DEVPOS(PROW(),24);DEVOUT(disco->data_pag)
                DEVPOS(PROW(),35);DEVOUT(TRANSFORM(disco->valor-disco->multa,'9,999.99'))
                DEVPOS(PROW(),45);DEVOUT(TRANSFORM(disco->multa,'9,999.99'))
                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(disco->valor,'9,999.99'))
                IF disco->flag = 'C'
                        mcart := mcart + disco->valor - disco->multa
*                       mcart := mcart + disco->valor
                        DEVPOS(PROW(),65);DEVOUT('COLEGIO')
                ELSE
                        mbanco := mbanco + disco->valor - disco->multa
*                       mbanco := mbanco + disco->valor
                        DEVPOS(PROW(),65);DEVOUT('BANCO')
                ENDIF
                mtotal := mtotal + disco->valor
                mmulta := mmulta + disco->multa
                mcont ++
                IF PROW() > 57
                        mpag ++
                        EJECT
                        cabecalho(mpag,mtit)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('GSTT')
                        DEVPOS(PROW(),06);DEVOUT('Matricula')
                        DEVPOS(PROW(),17);DEVOUT('Parc.')
                        DEVPOS(PROW(),24);DEVOUT('Data Pag.')
                        DEVPOS(PROW(),38);DEVOUT('Valor')
                        DEVPOS(PROW(),48);DEVOUT('Multa')
                        DEVPOS(PROW(),55);DEVOUT('Vlr.Pago')
                        DEVPOS(PROW(),65);DEVOUT('Tipo Pag.')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        imprt('M','N')
                ENDIF
                SKIP
        ENDDO
        DEVPOS(PROW()+2,00);DEVOUT('Total dos Pagantes..........: '+TRANSFORM(mcont,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes em BANCO...R$: '+TRANSFORM(mbanco,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes no COLEGIO R$: '+TRANSFORM(mcart,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total das mensalidades ...R$: '+TRANSFORM(mbanco+mcart,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total das multas..........R$: '+TRANSFORM(mmulta,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL GERAL ..............R$: '+TRANSFORM(mtotal,'9,999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        RETURN NIL
ENDDO
RETURN NIL
