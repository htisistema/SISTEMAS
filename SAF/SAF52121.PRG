************************************
* RELATORIO FINANCEIRO INDIVIDUAL
************************************

FUNCTION saf52121
*****************
LOCAL mprg:='SAF52121'
LOCAL tela,lci,cci,lba,cba,opcao,mtotal,mmulta,mbanco,;
      mcart,mextra,mcont,mtraco,mresp,mdata1,mdata2,mano
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35),mmatricula

IF ! ver_nivel(mprg,'RELATORIO FINANCEIRO INDIVIDUAL','125')
        RETURN NIL
ENDIF
lci := 15
cci := 10
lba := 20
cba := 75
tela := SAVESCREEN(01,00,24,79)
opcao := 'S'
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
mtraco := REPLI('-',137)
WHILE .T.
        mmatricula := SPACE(6)
        mtotal     := 0
        mmulta     := 0
        mcont      := 0
        mcart      := 0
        mbanco     := 0
        mextra     := 0
        mdata1     := CTOD('  /  /  ')
        mdata2     := CTOD('  /  /  ')
        mano       := SPACE(4)
        setcor(3)
        botao(lci,cci,lba,cba,,' Faturamento Individual ')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula...:')
        DEVPOS(lci+2,cci+1);DEVOUT('Data Inicial:')
        DEVPOS(lci+3,cci+1);DEVOUT('Data Final..:')
        DEVPOS(lci+4,cci+1);DEVOUT('Ano Compete.:')
        @ lci+1,cci+15 GET mmatricula PICT '999999' VALID ver_aluno(mmatricula,lci+1,cci+22)
        @ lci+2,cci+15 GET mdata1
        @ lci+3,cci+15 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        @ lci+4,cci+15 GET mano
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        setcor(3)
        DEVPOS(lci+1,cci+23);DEVOUT(aluno->aluno)
        setcor(1)
        *********************
        SELE('cdmen');ORDSETFOCUS(1)
        GO TOP
        *********************
        IF ! cdmen->(DBSEEK(mmatricula))
                atencao('Nao existe nenhuma parcela deste aluno')
                LOOP
        ENDIF
        IF ! imp_arq('REL_FINA.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'Relatorio Financeiro Individual'
        mtipo := 'Periodo: '+DTOC(mdata1)+' a '+DTOC(mdata2)
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('Matric')
        DEVPOS(PROW(),08);DEVOUT('Aluno')
        DEVPOS(PROW(),50);DEVOUT('GSTT')
        DEVPOS(PROW(),56);DEVOUT('Parc.')
        DEVPOS(PROW(),63);DEVOUT(' ANO')
        DEVPOS(PROW(),69);DEVOUT('Data Pag.')
        DEVPOS(PROW(),84);DEVOUT('Valor')
        DEVPOS(PROW(),94);DEVOUT('Multa')
        DEVPOS(PROW(),101);DEVOUT('Vlr.Pago')
        DEVPOS(PROW(),111);DEVOUT('Tipo Pag.')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT(cdmen->matricula)
        DEVPOS(PROW(),08);DEVOUT(aluno->aluno)
*        DEVPOS(PROW(),50);DEVOUT(cdmen->gstt)
        mresp := aluno->responsa
        WHILE mmatricula = cdmen->matricula .AND. ! EOF()
                IF ! EMPTY(mano) .AND. mano <> cdmen->ano
                        SKIP
                        LOOP
                ENDIF
                IF cdmen->data_pag < mdata1 .OR. cdmen->data_pag > mdata2
                        SKIP
                        LOOP
                ENDIF
                IF EMPTY(cdmen->data_pag)
                        SKIP
                        LOOP
                ENDIF
                DEVPOS(PROW(),50);DEVOUT(cdmen->gstt)
                DEVPOS(PROW(),58);DEVOUT(cdmen->parcela)
                DEVPOS(PROW(),63);DEVOUT(cdmen->ano)
                DEVPOS(PROW(),69);DEVOUT(cdmen->data_pag)
                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(cdmen->valor-cdmen->multa,'9,999.99'))
                DEVPOS(PROW(),91);DEVOUT(TRANSFORM(cdmen->multa,'9,999.99'))
                DEVPOS(PROW(),101);DEVOUT(TRANSFORM(cdmen->valor,'9,999.99'))
                IF cdmen->flag = 'C'
                        mcart := mcart + cdmen->valor - cdmen->multa
*                       mcart := mcart + cdmen->valor
                        DEVPOS(PROW(),111);DEVOUT('COLEGIO')
                ELSEIF cdmen->flag = 'B'
                        mbanco := mbanco + cdmen->valor - cdmen->multa
*                       mbanco := mbanco + cdmen->valor
                        DEVPOS(PROW(),111);DEVOUT('BANCO')
                ELSE
                        mextra := mextra + cdmen->valor - cdmen->multa
                        DEVPOS(PROW(),111);DEVOUT('EXTRA')
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
                        DEVPOS(PROW(),08);DEVOUT('Aluno')
                        DEVPOS(PROW(),50);DEVOUT('GSTT')
                        DEVPOS(PROW(),56);DEVOUT('Parc.')
                        DEVPOS(PROW(),63);DEVOUT(' ANO')
                        DEVPOS(PROW(),69);DEVOUT('Data Pag.')
                        DEVPOS(PROW(),84);DEVOUT('Valor')
                        DEVPOS(PROW(),94);DEVOUT('Multa')
                        DEVPOS(PROW(),101);DEVOUT('Vlr.Pago')
                        DEVPOS(PROW(),111);DEVOUT('Tipo Pag.')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(' ')
                SKIP
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT('Responsavel: '+mresp)
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Parcela Pagas......: '+TRANSFORM(mcont,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes em BANCO...R$: '+TRANSFORM(mbanco,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes no COLEGIO R$: '+TRANSFORM(mcart,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Pagantes EXTRAS.... R$: '+TRANSFORM(mextra,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total das mensalidades ...R$: '+TRANSFORM(mbanco+mcart,'9,999,999.99'))
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

