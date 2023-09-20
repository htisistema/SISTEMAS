************************************
* RELATORIO FINANCEIRO GERAL
************************************

FUNCTION saf52122
*****************
LOCAL mprg := 'SAF52122'
LOCAL tela,lci,cci,lba,cba,opcao,mtotal,mmulta,mbanco,mcart,mcont,mtraco,;
      mdata_ini,mdata_fim,mextra,mtipo_cons:=SPACE(1),mparcela,moutra_men:=0,;
      mexc_doc,mtipo_doc

LOCAL mtit,mtipo,mano
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE FINANCEIRO GERAL','125')
        RETURN NIL
ENDIF

lci := 13
cci := 10
lba := 21
cba := 65
tela := SAVESCREEN(01,00,24,79)
opcao := 'S'
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
mtraco := REPLI('-',137)
WHILE .T.
        mtotal := 0
        mmulta := 0
        mcont := 0
        mcart := 0
        mbanco := 0
        mextra := 0
        mdata_ini := CTOD('  /  /  ')
        mdata_fim := CTOD('  /  /  ')
        mtipo_cons := SPACE(1)
        mparcela := '  '
        mano := SPACE(4)
        mtipo_doc := SPACE(2)
        mexc_doc := SPACE(2)
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Data Inicial:')
        DEVPOS(lci+2,cci+1);DEVOUT('Data Final..:')
        DEVPOS(lci+3,cci+1);DEVOUT('Tipo [B]anco [C]arteira [E]xtra:')
        DEVPOS(lci+4,cci+1);DEVOUT('Digite a parcela que deseja....:')
        DEVPOS(lci+5,cci+1);DEVOUT('Digite o Ano que deseja........:')
        DEVPOS(lci+6,cci+1);DEVOUT('Digite o Tipo de Documento.....:')
        DEVPOS(lci+7,cci+1);DEVOUT('Tipo Doc.que Deseja Excluir....:')
        @ lci+1,cci+15 GET mdata_ini
        @ lci+2,cci+15 GET mdata_fim VALID IF(mdata_fim < mdata_ini,.F.,.T.)
        @ lci+3,cci+34 GET mtipo_cons PICT '@!' VALID mtipo_cons $ 'B,C,E, '
        @ lci+4,cci+34 GET mparcela PICT '@!'
        @ lci+5,cci+34 GET mano PICT '9999'
        @ lci+6,cci+34 GET mtipo_doc PICT '@!'
        @ lci+7,cci+34 GET mexc_doc PICT '@!' WHEN EMPTY(mtipo_doc)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        *********************
        SELE('cdmen');ORDSETFOCUS(10)
        GO TOP
        *********************
        cdmen->(DBSEEK(DTOS(mdata_ini),.T.))
        IF EOF()
                atencao('Nao existe nenhum pagamento neste periodo')
                LOOP
        ENDIF
        IF ! imp_arq('REL_FINA.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'Relatorio Financeiro GERAL'
        IF ! EMPTY(mano)
                mtipo := 'Periodo: '+DTOC(mdata_ini)+' a '+DTOC(mdata_fim)+' do Ano: '+mano
        ELSE
                mtipo := 'Periodo: '+DTOC(mdata_ini)+' a '+DTOC(mdata_fim)
        ENDIF
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('Matric')
        DEVPOS(PROW(),07);DEVOUT('Aluno')
        DEVPOS(PROW(),46);DEVOUT('GSTT')
        DEVPOS(PROW(),51);DEVOUT('Tp')
        DEVPOS(PROW(),54);DEVOUT('Pc')
        DEVPOS(PROW(),57);DEVOUT(' ANO')
        DEVPOS(PROW(),62);DEVOUT('Data Pag')
        DEVPOS(PROW(),74);DEVOUT('Valor')
        DEVPOS(PROW(),81);DEVOUT('Multa')
        DEVPOS(PROW(),89);DEVOUT('Vlr.Pg')
        DEVPOS(PROW(),96);DEVOUT('Loc. Pg')
        DEVPOS(PROW(),104);DEVOUT('Responsavel')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        moutra_men := 0
        WHILE cdmen->data_pag <= mdata_fim .AND. ! EOF()
                IF ! EMPTY(mano) .AND. mano <> cdmen->ano
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mparcela) .AND. mparcela <> cdmen->parcela
                        SKIP
                        LOOP
                ENDIF
                IF EMPTY(cdmen->data_pag)
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mtipo_cons) .AND. mtipo_cons <> cdmen->flag
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mexc_doc) .AND. mexc_doc = cdmen->tipo
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> cdmen->tipo
                        SKIP
                        LOOP
                ENDIF

                ver_aluno(cdmen->matricula,'*')
                DEVPOS(PROW()+1,00);DEVOUT(cdmen->matricula)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(LEFT(aluno->aluno,38))
                DEVPOS(PROW(),PCOL()+1);DEVOUT(cdmen->gstt)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(cdmen->tipo)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(cdmen->parcela)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(cdmen->ano)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(cdmen->data_pag)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(cdmen->valor-cdmen->multa,'9,999.99'))
                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(cdmen->multa,'999.99'))
                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(cdmen->valor,'9,999.99'))
                IF cdmen->flag = 'C'
                        mcart := mcart + cdmen->valor - cdmen->multa
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('COLEGIO')
                ELSEIF cdmen->flag = 'B'
                        mbanco := mbanco + cdmen->valor - cdmen->multa
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('BANCO  ')
                ELSE
                        mextra := mextra + cdmen->valor - cdmen->multa
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('EXTRA  ')
                ENDIF
                DEVPOS(PROW(),PCOL()+1);DEVOUT(LEFT(aluno->responsa,33))
                IF cdmen->tipo <> 'ME'
                        moutra_men := moutra_men + cdmen->valor
                ENDIF
                mtotal := mtotal + cdmen->valor
                mmulta := mmulta + cdmen->multa
                mcont ++
                IF PROW() > 57
                        mpag ++
                        EJECT
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT('Matric')
                        DEVPOS(PROW(),07);DEVOUT('Aluno')
                        DEVPOS(PROW(),46);DEVOUT('GSTT')
                        DEVPOS(PROW(),51);DEVOUT('Tp')
                        DEVPOS(PROW(),54);DEVOUT('Pc')
                        DEVPOS(PROW(),57);DEVOUT(' ANO')
                        DEVPOS(PROW(),62);DEVOUT('Data Pag')
                        DEVPOS(PROW(),74);DEVOUT('Valor')
                        DEVPOS(PROW(),81);DEVOUT('Multa')
                        DEVPOS(PROW(),89);DEVOUT('Vlr.Pg')
                        DEVPOS(PROW(),96);DEVOUT('Loc. Pg')
                        DEVPOS(PROW(),104);DEVOUT('Responsavel')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                SKIP
        ENDDO
        DEVPOS(PROW()+2,00);DEVOUT('Total de Parcela Pagas......: '+TRANSFORM(mcont,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes em BANCO...R$: '+TRANSFORM(mbanco,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes no COLEGIO R$: '+TRANSFORM(mcart,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes EXTRAS.....R$: '+TRANSFORM(mextra,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total das mensalidades ...R$: '+TRANSFORM(mbanco+mcart,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Outras Mensalidades.R$: '+TRANSFORM(moutra_men,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total das multas..........R$: '+TRANSFORM(mmulta,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL GERAL ..............R$: '+TRANSFORM(mtotal,'9,999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('REL_FINA.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL

