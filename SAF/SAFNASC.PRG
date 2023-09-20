**************************************
* RELACAO DE ALUNOS P/DATA NASCIMENTO
**************************************

FUNCTION safnasc
****************
LOCAL mprg:='SAFNASC'
LOCAL tela,lci,cci,lba,cba,opcao,mgstt,mdata_ini,mdata_fim,mcont_aluno,mpag,mtraco
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS P/DATA NASCIMENTO','15')
        RETURN NIL
ENDIF

lci := 10
cci := 10
lba := 14
cba := 60
tela := SAVESCREEN(00,00,24,79)
mtraco := REPLI('-',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        ****************************
        SELE('aluno');ORDSETFOCUS(7)
        GO TOP
        ****************************
        mgstt := SPACE(4)
        mdata_ini := SPACE(5)
        mdata_fim := SPACE(5)
        mcont_aluno := 0
        botao(lci,cci,lba,cba)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite o GSTT...............:')
        DEVPOS(lci+2,cci+1);DEVOUT('Digite Inicial <Aniversario>:')
        DEVPOS(lci+3,cci+1);DEVOUT('Digite Final   <Aniversario>:')
        @ lci+1,cci+30 GET mgstt PICT '@!' VALID ver_gstt(mgstt,,1)
        @ lci+2,cci+30 GET mdata_ini PICT '99/99' VALID DDMM(mdata_ini)
        @ lci+3,cci+30 GET mdata_fim PICT '99/99' VALID IF(VAL(SUBSTR(mdata_ini,4,2))>VAL(SUBSTR(mdata_fim,4,2)),.F.,DDMM(mdata_fim)) WHEN ! EMPTY(mdata_ini)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! aluno->(DBSEEK(mgstt)) .AND. ! EMPTY(mgstt)
                atencao('Nao existe nenhum aluno neste GSTT')
                LOOP
        ENDIF
        mensagem('Confirma a impressao do Relatorio [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('DTA_NASC.REL')
                LOOP
        ENDIF
        mrel := 'LPT1'
*       SET DEVI TO PRINT
*       SET PRINT TO 'LPT1'
        mtit := 'Relacao de Alunos p/ Data Nascimento'
        mtipo := 'Periodo: '+mdata_ini+' a '+mdata_fim
        mcont_aluno := 0
        mpag := 1
        cabecalho(mpag,mtit,mtipo)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('GSTT')
        DEVPOS(PROW(),05);DEVOUT('Nome')
        DEVPOS(PROW(),46);DEVOUT('Matricula')
        DEVPOS(PROW(),57);DEVOUT('Data Nasc.')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        IF ! EMPTY(mgstt)
                WHILE ! EOF() .AND. mgstt = aluno->gstt
                        IF CTOD(SUBSTR(DTOC(a_datan),1,5)+'/00') >= CTOD(mdata_ini+'/00') .AND. CTOD(SUBSTR(DTOC(a_datan),1,5)+'/00') <= CTOD(mdata_fim+'/00')
                                DEVPOS(PROW()+1,00);DEVOUT(aluno->gstt+' '+aluno->aluno+'  '+aluno->matricula+'     '+DTOC(aluno->a_datan))
                                mcont_aluno ++
                                SKIP
                                IF PROW() >= 57
                                        mpag ++
                                        EJECT
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT('GSTT')
                                        DEVPOS(PROW(),05);DEVOUT('Nome')
                                        DEVPOS(PROW(),46);DEVOUT('Matricula')
                                        DEVPOS(PROW(),57);DEVOUT('Data Nasc.')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                ENDIF
                        ELSE
                                SKIP
                        ENDIF
                ENDDO
        ELSE
                ****************************
                SELE('aluno');ORDSETFOCUS(8)
                GO TOP
                ****************************
                WHILE ! EOF()
                        IF CTOD(SUBSTR(DTOC(a_datan),1,5)+'/00') >= CTOD(mdata_ini+'/00') .AND. CTOD(SUBSTR(DTOC(a_datan),1,5)+'/00') <= CTOD(mdata_fim+'/00')
                                DEVPOS(PROW()+1,00);DEVOUT(aluno->gstt+' '+aluno->aluno+'  '+aluno->matricula+'     '+DTOC(aluno->a_datan))
                                mcont_aluno ++
                                SKIP
                                IF PROW() >= 57
                                        mpag ++
                                        EJECT
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT('GSTT')
                                        DEVPOS(PROW(),05);DEVOUT('Nome')
                                        DEVPOS(PROW(),46);DEVOUT('Matricula')
                                        DEVPOS(PROW(),57);DEVOUT('Data Nasc.')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                ENDIF
                        ELSE
                                SKIP
                        ENDIF
                ENDDO
        ENDIF
        DEVPOS(PROW()+2,00);DEVOUT('Quantidade de Alunos: '+STRZERO(mcont_aluno,6))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('DAT_NASC.REL')
        ENDIF
ENDDO
RETURN NIL
