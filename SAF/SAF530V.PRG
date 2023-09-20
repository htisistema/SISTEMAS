************************************************
* RELATORIO DE ALUNOS
************************************************

FUNCTION saf530
***************
LOCAL mprg:='SAF530'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,;
      tela,mgst,mmat,mop_tp,mfaixa,mflag,mimp_resp:=' ',mcont_ind:=0,mqtd_parc:=0
LOCAL mesporte,mtransp,mcurso,mtotal:=0,mcab,mcont,msele,morde,mtot_esp:=0
PRIVATE m_parcela:={},mquantd_parc
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS ','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',132)
lci := 06
cci := 10
lba := 15
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mop_tp := 0
        mgst := SPACE(4)
        mmat := SPACE(6)
        mquantd_parc := 0
        mindex := 0
        mtot_men := 0
        mtot_ger := 0
        mcont_aluno := 0
        mcont_parc := 0
        mmes := SPACE(2)
        mquantd = 1
        mtit := 'ANEXO I'
        mesporte := 0
        mtransp  := 0
        mcurso   := 0
        mtotal   := 0
        mcab     := 'S'
        mcont    := 0
        mtot_esp := 0
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite o MES que deseja...............................: ')
        DEVPOS(lci+6,cci+1);DEVOUT('Opcao [C]om Cabecalho [S]em Cabecalho.................: ')
        DEVPOS(lci+8,cci+1);DEVOUT('Titulo do Relatorio: ')
        @ lci+1,cci+1 PROMPT ' P/GSTT ' MESSAGE '** Relacao por ordem de GSTT - <ESC> Retornar **'
        @ lci+1,cci+11 PROMPT ' P/CONTRATO ' MESSAGE '** Relacao por ordem de Contrato - <ESC> Retornar **'
        @ lci+1,cci+26 PROMPT ' P/ALUNO ' MESSAGE '** Relacao por ordem alfabetica - <ESC> p/retornar **'
        SET INTEN ON
        MENU TO mop_tp
        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF mop_tp = 1
                DEVPOS(lci+3,cci+1);DEVOUT('Digite a GSTT : ')
                @ lci+3,cci+17 GET mgst PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                IF ! aluno->(DBSEEK(mgst))
                        atencao('Nao foi encontrado esta GSTT')
                        LOOP
                ENDIF
        ENDIF
        mcab := 'S'
        @ lci+5,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+6,cci+57 GET mcab PICT '@!' VALID mcab $ 'S,N'
        @ lci+8,cci+22 GET mtit
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF

        IF ! imp_arq('RELA_ALU.REL')
                LOOP
        ENDIF
        mpag := 1
        mtipo := ' '
        IF mcab = 'S'
                cabecalho(mpag,mtit,mtipo)
                DEVPOS(PROW()+1,00);DEVOUT('Dados')
                DEVPOS(PROW()+1,00);DEVOUT('Cadastrais')
                DEVPOS(PROW()+1,00);DEVOUT('Raz∆o Social: Sociedade Civil Colegio Nobrega')
                DEVPOS(PROW()+1,00);DEVOUT('CNPJ: 10.977.585/0001-58')
                DEVPOS(PROW()+1,00);DEVOUT('CMC:  002.912-2')
                DEVPOS(PROW()+1,00);DEVOUT('Mes: '+mmes)
                DEVPOS(PROW()+1,00);DEVOUT('Ano: '+mdiretorio)
        ENDIF
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('N.Contrato')
        DEVPOS(PROW(),12);DEVOUT('Aluno')
        DEVPOS(PROW(),53);DEVOUT('Serie')
        DEVPOS(PROW(),59);DEVOUT('Grau')
        DEVPOS(PROW(),64);DEVOUT('Turma')
        DEVPOS(PROW(),71);DEVOUT('Mensalidade')
        DEVPOS(PROW(),84);DEVOUT('Ativ.Esportivas')
        DEVPOS(PROW(),101);DEVOUT('Transportes')
        DEVPOS(PROW(),114);DEVOUT('Cursos Extras')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        imprt('M','C')

        IF mop_tp = 3
                ***************
                SELE('aluno');ORDSETFOCUS(3)
                GO TOP
                ***************
                mtotal := 0
                WHILE ! EOF()
                        IF aluno->gstt = 'DEBI'
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := 0
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->contrato)
                        DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),55);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),60);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),66);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        mvlr_men := vlr_men(mgstt,mmes)
                        DEVPOS(PROW(),72);DEVOUTPICT(mvlr_men,'999,999.99')
                        msele := SELE();morde:=INDEXORD()
                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        mesporte := 0
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE aluno->matricula = cdmen->matricula .AND. ! EOF()
                                        IF cdmen->ano <> mdiretorio .OR. cdmen->tipo = 'ME' .OR. cdmen->parcela <> mmes
                                                SKIP
                                                LOOP
                                        ENDIF
                                        mesporte := mesporte + cdmen->vlr_parc
                                        mtot_esp := mtot_esp + cdmen->vlr_parc
                                        SKIP
                                ENDDO
                        ENDIF
                        SELE(msele);ORDSETFOCUS(morde)
                        DEVPOS(PROW(),89);DEVOUTPICT(mesporte,'999,999.99')
                        DEVPOS(PROW(),102);DEVOUT(mtransp)
                        DEVPOS(PROW(),117);DEVOUT(mcurso)
                        mtotal := mtotal + mvlr_men
                        SKIP

                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                mtipo := ' '
                                cabecalho(mpag,mtit,mtipo)
                                DEVPOS(PROW()+1,00);DEVOUT('Dados')
                                DEVPOS(PROW()+1,00);DEVOUT('Cadastrais')
                                DEVPOS(PROW()+1,00);DEVOUT('Raz∆o Social: Sociedade Civil Colegio Nobrega')
                                DEVPOS(PROW()+1,00);DEVOUT('CNPJ: 10.977.585/0001-58')
                                DEVPOS(PROW()+1,00);DEVOUT('CMC:  002.912-2')
                                DEVPOS(PROW()+1,00);DEVOUT('Mes: '+mmes)
                                DEVPOS(PROW()+1,00);DEVOUT('Ano: '+mdiretorio)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('N.Contrato')
                                DEVPOS(PROW(),12);DEVOUT('Aluno')
                                DEVPOS(PROW(),53);DEVOUT('Serie')
                                DEVPOS(PROW(),59);DEVOUT('Grau')
                                DEVPOS(PROW(),64);DEVOUT('Turma')
                                DEVPOS(PROW(),71);DEVOUT('Mensalidade')
                                DEVPOS(PROW(),84);DEVOUT('Ativ.Esportivas')
                                DEVPOS(PROW(),101);DEVOUT('Transportes')
                                DEVPOS(PROW(),114);DEVOUT('Cursos Extras')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral R$:')
                DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mtotal,'999,999.99'))
                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mtot_esp,'999,999.99'))
                DEVPOS(PROW(),102);DEVOUT(mtransp)
                DEVPOS(PROW(),117);DEVOUT(mcurso)

        ELSEIF mop_tp = 2
                ***************
                SELE('cdmen');ORDSETFOCUS(9)
                GO TOP
                ***************
                IF ! cdmen->(DBSEEK(mmes))
                        atencao('Nao existe nenhum aluno neste mes')
                        LOOP
                ENDIF
                mtotal := 0
                WHILE ! EOF() .AND. mmes = cdmen->parcela
                        IF ! ver_aluno(cdmen->matricula,'*')
                                SKIP
                                LOOP
                        ENDIF
                        IF aluno->gstt = 'DEBI' .OR. cdmen->ano <> mdiretorio
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2)) .AND. EMPTY(cdmen->data_pag)
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := 0

                        mmat_aux := cdmen->matricula

                        DEVPOS(PROW()+1,00);DEVOUT(cdmen->contrato)
                        DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),55);DEVOUT(SUBSTR(cdmen->gstt,2,1))
                        DEVPOS(PROW(),60);DEVOUT(SUBSTR(cdmen->gstt,1,1))
                        DEVPOS(PROW(),66);DEVOUT(SUBSTR(cdmen->gstt,3,1))
*                       mvlr_men := vlr_men(mgstt,mmes)
                        DEVPOS(PROW(),72);DEVOUT(cdmen->vlr_parc)
                        mesporte := 0
                        WHILE mmat_aux = cdmen->matricula .AND. ! EOF()
                                IF cdmen->ano <> mdiretorio .OR. cdmen->tipo = 'ME' .OR. cdmen->parcela <> mmes
                                        SKIP
                                        LOOP
                                ENDIF
                                mesporte := mesporte + cdmen->vlr_parc
                                mtot_esp := mtot_esp + cdmen->vlr_parc
                                SKIP
                        ENDDO
                        DEVPOS(PROW(),89);DEVOUTPICT(mesporte,'999,999.99')
                        DEVPOS(PROW(),102);DEVOUT(mtransp)
                        DEVPOS(PROW(),117);DEVOUT(mcurso)
                        mtotal := mtotal + cdmen->vlr_parc
                        mcont ++
*                       SKIP
                        IF PROW() >= 59
                                IF mcab = 'S'
                                        EJECT
                                        mpag ++
                                        mtipo := ' '
                                        cabecalho(mpag,mtit,mtipo)
                                        DEVPOS(PROW()+1,00);DEVOUT('Dados')
                                        DEVPOS(PROW()+1,00);DEVOUT('Cadastrais')
                                        DEVPOS(PROW()+1,00);DEVOUT('Raz∆o Social: Sociedade Civil Colegio Nobrega')
                                        DEVPOS(PROW()+1,00);DEVOUT('CNPJ: 10.977.585/0001-58')
                                        DEVPOS(PROW()+1,00);DEVOUT('CMC:  002.912-2')
                                        DEVPOS(PROW()+1,00);DEVOUT('Mes: '+mmes)
                                        DEVPOS(PROW()+1,00);DEVOUT('Ano: '+mdiretorio)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('N.Contrato')
                                        DEVPOS(PROW(),12);DEVOUT('Aluno')
                                        DEVPOS(PROW(),53);DEVOUT('Serie')
                                        DEVPOS(PROW(),59);DEVOUT('Grau')
                                        DEVPOS(PROW(),64);DEVOUT('Turma')
                                        DEVPOS(PROW(),71);DEVOUT('Mensalidade')
                                        DEVPOS(PROW(),84);DEVOUT('Ativ.Esportivas')
                                        DEVPOS(PROW(),101);DEVOUT('Transportes')
                                        DEVPOS(PROW(),114);DEVOUT('Cursos Extras')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        imprt('M','C')
                                ENDIF
                                LOOP
                        ENDIF
                ENDDO
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral R$:')
                DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mtotal,'999,999.99'))
                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mtot_esp,'999,999.99'))
                DEVPOS(PROW(),102);DEVOUT(mtransp)
                DEVPOS(PROW(),117);DEVOUT(mcurso)
                DEVPOS(PROW()+1,00);DEVOUT('Qtd.Alunos....:')
                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mcont,'999,999.99'))
        ELSEIF mop_tp = 1
                mtotal := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        IF ! EMPTY(aluno->dt_tranf)
                                IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        mvlr_men := 0
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->contrato)
                        DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),55);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),60);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),66);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        mvlr_men := vlr_men(mgstt,mmes)
                        DEVPOS(PROW(),72);DEVOUT(mvlr_men)
                        msele := SELE();morde:=INDEXORD()
                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        mesporte := 0
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE aluno->matricula = cdmen->matricula .AND. ! EOF()
                                        IF cdmen->ano <> mdiretorio .OR. cdmen->tipo = 'ME' .OR. cdmen->parcela <> mmes
                                                SKIP
                                                LOOP
                                        ENDIF
                                        mesporte := mesporte + cdmen->vlr_parc
                                        mtot_esp := mtot_esp + cdmen->vlr_parc
                                        SKIP
                                ENDDO
                        ENDIF
                        SELE(msele);ORDSETFOCUS(morde)
                        DEVPOS(PROW(),89);DEVOUTPICT(mesporte,'999,999.99')
                        DEVPOS(PROW(),102);DEVOUT(mtransp)
                        DEVPOS(PROW(),117);DEVOUT(mcurso)
                        mtotal := mtotal + mvlr_men
                        SKIP

                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                mtipo := ' '
                                cabecalho(mpag,mtit,mtipo)
                                DEVPOS(PROW()+1,00);DEVOUT('Dados')
                                DEVPOS(PROW()+1,00);DEVOUT('Cadastrais')
                                DEVPOS(PROW()+1,00);DEVOUT('Raz∆o Social: Sociedade Civil Colegio Nobrega')
                                DEVPOS(PROW()+1,00);DEVOUT('CNPJ: 10.977.585/0001-58')
                                DEVPOS(PROW()+1,00);DEVOUT('CMC:  002.912-2')
                                DEVPOS(PROW()+1,00);DEVOUT('Mes: '+mmes)
                                DEVPOS(PROW()+1,00);DEVOUT('Ano: '+mdiretorio)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('N.Contrato')
                                DEVPOS(PROW(),12);DEVOUT('Aluno')
                                DEVPOS(PROW(),53);DEVOUT('Serie')
                                DEVPOS(PROW(),59);DEVOUT('Grau')
                                DEVPOS(PROW(),64);DEVOUT('Turma')
                                DEVPOS(PROW(),71);DEVOUT('Mensalidade')
                                DEVPOS(PROW(),84);DEVOUT('Ativ.Esportivas')
                                DEVPOS(PROW(),101);DEVOUT('Transportes')
                                DEVPOS(PROW(),114);DEVOUT('Cursos Extras')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral R$:')
                DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mtotal,'999,999.99'))
                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mtot_esp,'999,999.99'))
                DEVPOS(PROW(),102);DEVOUT(mtransp)
                DEVPOS(PROW(),117);DEVOUT(mcurso)
        ENDIF

        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('RELA_ALU.REL')
        ENDIF
ENDDO
RETURN NIL

