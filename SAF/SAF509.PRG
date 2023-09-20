************************************************
* EMISSAO DE FICHAS INDIVIDUAIS
************************************************

FUNCTION saf509(mconsulta)
***************
LOCAL mprg:='SAF509'
LOCAL lci,cci,lba,cba,opcao,tela,i,mmatricula,mgstt,mtipo_con,mtransf,maluno
PRIVATE m_nota := {},mtipo_sit,munidade,mcont_rec

IF ! ver_nivel(mprg,'RELATORIO DE FICHAS INDIVIDUAIS','125')
        RETURN NIL
ENDIF

IF mconsulta = NIL
        *--------------------------------------------------------------------------
        CLOSE ALL
        IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
        IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
        IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
        IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
        *--------------------------------------------------------------------------
ENDIF
lci := 09
cci := 08
lba := 13
cba := 70
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mtipo_sit := SPACE(20)
        munidade := 'MF'
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mtipo_con := 0
        mgstt := SPACE(4)
        mcont_rec := 0
        IF mconsulta = NIL
                @ 23,00 CLEAR TO 23,79
                setcor(3)
                botao(lci,cci,lba,cba)
                janela(lci,cba,' Emissao de Boletim ','*')
                @ lci+2,cci+1 TO lci+2,cba-1
                setcor(1)
                @ lci+1,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de Boletim INDIVIDUAL'
                @ lci+1,COL()+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim por Grau, Serie, Turma e turno'
                @ lci+1,COL()+4 PROMPT '    Geral   ' MESSAGE 'Emissao de Boletim em GERAL'
                SET INTEN ON
                MENU TO mtipo_con

                IF LASTKEY() = 27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RETURN NIL
                ENDIF
        ELSE
                mtipo_con := 1
        ENDIF
        IF mtipo_con = 1
                IF mconsulta = NIL
                        *****************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        *****************
                        DEVPOS(lci+3,cci+1);DEVOUT('Matricula:')
                        @ lci+3,cci+12 GET mmatricula PICT '999999'
                        @ lci+3,cci+19 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.) WHEN EMPTY(mmatricula)
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        IF ! EMPTY(mmatricula)
                                ORDSETFOCUS(4)
                                IF ! aluno->(DBSEEK(mmatricula))
                                        atencao('Matricula nao encontrada no arquivo')
                                        LOOP
                                ENDIF
                        ELSEIF ! EMPTY(maluno)
                                ORDSETFOCUS(3)
                                IF ! aluno->(DBSEEK(RTRIM(maluno)))
                                        atencao('Aluno nao encontrada no arquivo')
                                        LOOP
                                ENDIF
                                mmatricula := f7_aluno()
                                IF LASTKEY() = 27
                                        LOOP
                                ENDIF
                        ELSE
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+3,cci+12);DEVOUT(mmatricula)
                        DEVPOS(lci+3,cci+19);DEVOUT(aluno->aluno)
                        setcor(1)
                        mensagem('Confirma a Impressao [S/n]:')
                        opcao = 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF opcao = 'N'
                                LOOP
                        ENDIF
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                ELSE
                        mensagem('Espere o Final da Consulta OK - [ESC]Abandonar')
                ENDIF
                SET DEVI TO PRINT
                IF mconsulta <> NIL
                        SET PRINT TO ficha.rel
                ENDIF
                imprt('M','E')
                DEVPOS(PROW(),00);DEVOUT(memp+resa)
                DEVPOS(PROW(),25);DEVOUT('ANO - '+mdiretorio)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT(mend_firm)
                DEVPOS(PROW(),86);DEVOUT('Aluno: '+aluno->aluno)
                DEVPOS(PROW()+1,00);DEVOUT(mfone_firm)
                DEVPOS(PROW(),86);DEVOUT('Matricula: '+aluno->matricula+'  Serie: '+SUBSTR(aluno->gstt,2,1)+'  Turno: '+SUBSTR(aluno->gstt,3,1)+'  Turma: '+SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                DEVPOS(PROW(),86);DEVOUT('Data Nasc.: '+DTOC(aluno->a_datan)+'  Cidade: '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado)
                DEVPOS(PROW()+1,00);DEVOUT(mcgc_firm)
                DEVPOS(PROW(),86);DEVOUT('Pai: '+aluno->pai)
                DEVPOS(PROW()+1,00);DEVOUT(minsc_firm)
                DEVPOS(PROW(),86);DEVOUT('Mae: '+aluno->mae)
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                imprt('M','C',1)
                imprt('M','E')
                DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,1,1)+' GRAU - FICHA INDIVIDUAL')
                imprt('M','C',1)

                DEVPOS(PROW(),00);DEVOUT(REPLI('-',132))

                DEVPOS(PROW()+1,25);DEVOUT('|     |   I UNIDADE |  II UNIDADE | III UNIDADE |  IV UNIDADE |                AVALIACAO FINAL             ')
                DEVPOS(PROW()+1,25);DEVOUT('|     |-------------|-------------|-------------|-------------|--------------------------------------------')
                DEVPOS(PROW()+1,05);DEVOUT('DISCIPLINA')
                DEVPOS(PROW(),25);DEVOUT('| CH. |  MED. | FA  |  MED. | FA  |  MED. | FA  |  MED. | FA  | T.PT | MED. | N.R.A | MED.R | N.R.F | MED.F ')

                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                ASIZE(m_nota,0)
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                boletim(aluno->matricula)
                i := 0
                FOR i = 1 TO LEN(m_nota)
                        DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,2])
                        DEVPOS(PROW(),25);DEVOUT('|')
                        DEVPOS(PROW(),26);DEVOUT(TRANSFORM(ver_carga(aluno->gstt,m_nota[i,1]),'99999'))
                        DEVPOS(PROW(),31);DEVOUT('|')
                        IF m_nota[i,18] = 'F'
                                DEVPOS(PROW(),33);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                DEVPOS(PROW(),33);DEVOUT('  D ')
                        ELSEIF ! EMPTY(m_nota[i,22])
                                DEVPOS(PROW(),33);DEVOUT('  '+m_nota[i,22]+' ')
                        ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),33);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),33);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                        ENDIF
                        DEVPOS(PROW(),39);DEVOUT('|')
*                       DEVPOS(PROW(),41);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                        DEVPOS(PROW(),41);DEVOUT('   ')    //falta1
                        DEVPOS(PROW(),45);DEVOUT('|')
                        IF m_nota[i,19] = 'F'
                                DEVPOS(PROW(),47);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                DEVPOS(PROW(),47);DEVOUT('  D ')
                        ELSEIF ! EMPTY(m_nota[i,23])
                                DEVPOS(PROW(),47);DEVOUT('  '+m_nota[i,23]+' ')
                        ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),47);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),47);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                        ENDIF
                        DEVPOS(PROW(),53);DEVOUT('|')
*                       DEVPOS(PROW(),55);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                        DEVPOS(PROW(),55);DEVOUT('   ')    //falta2
                        DEVPOS(PROW(),59);DEVOUT('|')
                        IF m_nota[i,20] = 'F'
                                DEVPOS(PROW(),61);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                DEVPOS(PROW(),61);DEVOUT('  D ')
                        ELSEIF ! EMPTY(m_nota[i,24])
                                DEVPOS(PROW(),61);DEVOUT('  '+m_nota[i,24]+' ')
                        ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),61);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),61);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                        ENDIF
                        DEVPOS(PROW(),67);DEVOUT('|')
*                       DEVPOS(PROW(),69);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                        DEVPOS(PROW(),69);DEVOUT('   ')    //falta3
                        DEVPOS(PROW(),73);DEVOUT('|')
                        IF m_nota[i,21] = 'F'
                                DEVPOS(PROW(),75);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                DEVPOS(PROW(),75);DEVOUT('  D ')
                        ELSEIF ! EMPTY(m_nota[i,25])
                                DEVPOS(PROW(),75);DEVOUT('  '+m_nota[i,25]+' ')
                        ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),75);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),75);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                        ENDIF
                        DEVPOS(PROW(),81);DEVOUT('|')
*                       DEVPOS(PROW(),82);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                        DEVPOS(PROW(),82);DEVOUT('   ')   //falta4
                        DEVPOS(PROW(),87);DEVOUT('|')
                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(m_nota[i,3]+m_nota[i,5]+m_nota[i,7]+m_nota[i,9],'99.9'))   //TOTAL DAS MEDIAS
                        DEVPOS(PROW(),94);DEVOUT('|')
                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                DEVPOS(PROW(),96);DEVOUT('  D ')
                        ELSE
                                DEVPOS(PROW(),96);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                        ENDIF
                        // recuperacao anual
                        DEVPOS(PROW(),101);DEVOUT('|')
                        DEVPOS(PROW(),103);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                        // media anual
                        DEVPOS(PROW(),109);DEVOUT('|')
                        DEVPOS(PROW(),111);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                        // recuperacao final
                        DEVPOS(PROW(),117);DEVOUT('|')
                        DEVPOS(PROW(),119);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                        // media final
                        DEVPOS(PROW(),125);DEVOUT('|')
                        DEVPOS(PROW(),127);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                        // total das faltas
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                        // situacao
/*
                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSEIF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                        ENDIF
*/
                        IF m_nota[i,17] <> 'A'
                                IF m_nota[i,17] = 'RA'
                                        mtipo_sit := 'RECUPERACAO'
                                ELSEIF m_nota[i,17] = 'RF'
                                        mtipo_sit := 'RECUPERACAO FINAL'
                                ELSE
                                        mtipo_sit := 'REPROVADO'
                                ENDIF
                        ENDIF
                NEXT
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',132))
*               IF mcont_rec > 4
*                       DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: R E P R O V A D O cont')
*               ELSEIF mtipo_sit = 'A'
                IF mtipo_sit = 'A'
                        DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: A P R O V A D O')
                ELSEIF mtransf = 'S'
                        DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: TRANSFERIDO')
                ELSE
                        DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: R E P R O V A D O')
                ENDIF
                imprt('M','N',4)
                DEVPOS(PROW(),00);DEVOUT(PADC(sis_cidade+', '+imp_data(mdata_sis),80))
                DEVPOS(PROW()+6,00);DEVOUT(PADC('-----------------------------',80))
                DEVPOS(PROW()+1,00);DEVOUT(PADC('          SECRETARIA          ',80))
                imprt('M','N',4)
                SETPRC(00,00)
                IF mconsulta = NIL
                        EJECT
                ENDIF
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                IF mconsulta <> NIL
                        botao(02,00,23,79)
                        janela(02,79,' Consulta da Ficha Individual ')
                        mensagem('<TAB>p/Direita - <HOME>p/Esquerda - <ESC>p/abandonar')
                        mconsulta := MEMOREAD('ficha.rel')
                        MEMOEDIT(mconsulta,03,01,22,78,.F.,,160,80)
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                LOOP
        ELSEIF mtipo_con = 2
                *****************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                *****************
                DEVPOS(lci+3,cci+1);DEVOUT('GSTT:')
                @ lci+3,COL()+2 GET mgstt PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mgstt))
                        atencao('Nao foi possivel encontrar esta TURMA')
                        LOOP
                ENDIF
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        LOOP
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                WHILE mgstt = aluno->gstt .AND. ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        imprt('M','E')
                        DEVPOS(PROW(),00);DEVOUT(memp+resa)
                        DEVPOS(PROW(),25);DEVOUT('ANO - '+mdiretorio)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(mend_firm)
                        DEVPOS(PROW(),86);DEVOUT('Aluno: '+aluno->aluno)
                        DEVPOS(PROW()+1,00);DEVOUT(mfone_firm)
                        DEVPOS(PROW(),86);DEVOUT('Matricula: '+aluno->matricula+'  Serie: '+SUBSTR(aluno->gstt,2,1)+'  Turno: '+SUBSTR(aluno->gstt,3,1)+'  Turma: '+SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                        DEVPOS(PROW(),86);DEVOUT('Data Nasc.: '+DTOC(aluno->a_datan)+'  Cidade: '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado)
                        DEVPOS(PROW()+1,00);DEVOUT(mcgc_firm)
                        DEVPOS(PROW(),86);DEVOUT('Pai: '+aluno->pai)
                        DEVPOS(PROW()+1,00);DEVOUT(minsc_firm)
                        DEVPOS(PROW(),86);DEVOUT('Mae: '+aluno->mae)
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        imprt('M','C',1)
                        imprt('M','E')
                        DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,1,1)+' GRAU - FICHA INDIVIDUAL')
                        imprt('M','C',1)

                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',132))

                        DEVPOS(PROW()+1,25);DEVOUT('|     |   I UNIDADE |  II UNIDADE | III UNIDADE |  IV UNIDADE |                AVALIACAO FINAL             ')
                        DEVPOS(PROW()+1,25);DEVOUT('|     |-------------|-------------|-------------|-------------|--------------------------------------------')
                        DEVPOS(PROW()+1,05);DEVOUT('DISCIPLINA')
                        DEVPOS(PROW(),25);DEVOUT('| CH. |  MED. | FA  |  MED. | FA  |  MED. | FA  |  MED. | FA  | T.PT | MED. | N.R.A | MED.R | N.R.F | MED.F ')

                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,2])
                                DEVPOS(PROW(),25);DEVOUT('|')
                                DEVPOS(PROW(),26);DEVOUT(TRANSFORM(ver_carga(aluno->gstt,m_nota[i,1]),'99999'))
                                DEVPOS(PROW(),31);DEVOUT('|')
*                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,2])
*                                DEVPOS(PROW(),25);DEVOUT('|     |')
                                IF m_nota[i,18] = 'F'
                                        DEVPOS(PROW(),33);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),33);DEVOUT('  D ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),33);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),33);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),33);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                ENDIF
                                DEVPOS(PROW(),39);DEVOUT('|')
*                               DEVPOS(PROW(),41);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                                DEVPOS(PROW(),41);DEVOUT('   ')    //falta1
                                DEVPOS(PROW(),45);DEVOUT('|')
                                IF m_nota[i,19] = 'F'
                                        DEVPOS(PROW(),47);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),47);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),47);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),47);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                ENDIF
                                DEVPOS(PROW(),53);DEVOUT('|')
*                               DEVPOS(PROW(),55);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                                DEVPOS(PROW(),55);DEVOUT('   ')    //falta2
                                DEVPOS(PROW(),59);DEVOUT('|')
                                IF m_nota[i,20] = 'F'
                                        DEVPOS(PROW(),61);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),61);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),61);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),61);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                ENDIF
                                DEVPOS(PROW(),67);DEVOUT('|')
*                               DEVPOS(PROW(),69);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                                DEVPOS(PROW(),69);DEVOUT('   ')    //falta3
                                DEVPOS(PROW(),73);DEVOUT('|')
                                IF m_nota[i,21] = 'F'
                                        DEVPOS(PROW(),75);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),75);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),75);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),75);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                ENDIF
                                DEVPOS(PROW(),81);DEVOUT('|')
*                               DEVPOS(PROW(),82);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                                DEVPOS(PROW(),82);DEVOUT('   ')   //falta4
                                DEVPOS(PROW(),87);DEVOUT('|')
                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(m_nota[i,3]+m_nota[i,5]+m_nota[i,7]+m_nota[i,9],'99.9'))   //TOTAL DAS MEDIAS
                                DEVPOS(PROW(),94);DEVOUT('|')
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),96);DEVOUT('  D ')
                                ELSE
                                        DEVPOS(PROW(),96);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                                ENDIF
                                // recuperacao anual
                                DEVPOS(PROW(),101);DEVOUT('|')
                                DEVPOS(PROW(),103);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                // media anual
                                DEVPOS(PROW(),109);DEVOUT('|')
                                DEVPOS(PROW(),111);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                // recuperacao final
                                DEVPOS(PROW(),117);DEVOUT('|')
                                DEVPOS(PROW(),119);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                // media final
                                DEVPOS(PROW(),125);DEVOUT('|')
                                DEVPOS(PROW(),127);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                // total das faltas
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                                // situacao
/*
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                                ENDIF
*/
                                IF m_nota[i,17] <> 'A'
                                        IF m_nota[i,17] = 'RA'
                                                mtipo_sit := 'RECUPERACAO'
                                        ELSEIF m_nota[i,17] = 'RF'
                                                mtipo_sit := 'RECUPERACAO FINAL'
                                        ELSE
                                                mtipo_sit := 'REPROVADO'
                                        ENDIF
                                ENDIF
                        NEXT
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',132))
                        IF mtipo_sit =='A'
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: A P R O V A D O')
                        ELSEIF mtransf = 'S'
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: TRANSFERIDO')
                        ELSE
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: R E P R O V A D O')
                        ENDIF
                        imprt('M','N',4)
                        DEVPOS(PROW(),00);DEVOUT(PADC(sis_cidade+', '+imp_data(mdata_sis),80))
                        DEVPOS(PROW()+6,00);DEVOUT(PADC('-----------------------------',80))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('          SECRETARIA          ',80))
                        imprt('M','N',4)
                        SETPRC(00,00)
                        EJECT
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSE
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF opcao = 'N'
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                WHILE ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        imprt('M','E')
                        DEVPOS(PROW(),00);DEVOUT(memp+resa)
                        DEVPOS(PROW(),25);DEVOUT('ANO - '+mdiretorio)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(mend_firm)
                        DEVPOS(PROW(),86);DEVOUT('Aluno: '+aluno->aluno)
                        DEVPOS(PROW()+1,00);DEVOUT(mfone_firm)
                        DEVPOS(PROW(),86);DEVOUT('Matricula: '+aluno->matricula+'  Serie: '+SUBSTR(aluno->gstt,2,1)+'  Turno: '+SUBSTR(aluno->gstt,3,1)+'  Turma: '+SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm)
                        DEVPOS(PROW(),86);DEVOUT('Data Nasc.: '+DTOC(aluno->a_datan)+'  Cidade: '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado)
                        DEVPOS(PROW()+1,00);DEVOUT(mcgc_firm)
                        DEVPOS(PROW(),86);DEVOUT('Pai: '+aluno->pai)
                        DEVPOS(PROW()+1,00);DEVOUT(minsc_firm)
                        DEVPOS(PROW(),86);DEVOUT('Mae: '+aluno->mae)
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        imprt('M','C',1)
                        imprt('M','E')
                        DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,1,1)+' GRAU - FICHA INDIVIDUAL')
                        imprt('M','C',1)

                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',132))

                        DEVPOS(PROW()+1,25);DEVOUT('|     |   I UNIDADE |  II UNIDADE | III UNIDADE |  IV UNIDADE |                AVALIACAO FINAL             ')
                        DEVPOS(PROW()+1,25);DEVOUT('|     |-------------|-------------|-------------|-------------|--------------------------------------------')
                        DEVPOS(PROW()+1,05);DEVOUT('DISCIPLINA')
                        DEVPOS(PROW(),25);DEVOUT('| CH. |  MED. | FA  |  MED. | FA  |  MED. | FA  |  MED. | FA  | T.PT | MED. | N.R.A | MED.R | N.R.F | MED.F ')

                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,2])
                                DEVPOS(PROW(),25);DEVOUT('|')
                                DEVPOS(PROW(),26);DEVOUT(TRANSFORM(ver_carga(aluno->gstt,m_nota[i,1]),'99999'))
                                DEVPOS(PROW(),31);DEVOUT('|')
*                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,2])
*                                DEVPOS(PROW(),25);DEVOUT('|     |')
                                IF m_nota[i,18] = 'F'
                                        DEVPOS(PROW(),33);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),33);DEVOUT('  D ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),33);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),33);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),33);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                ENDIF
                                DEVPOS(PROW(),39);DEVOUT('|')
*                               DEVPOS(PROW(),41);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                                DEVPOS(PROW(),41);DEVOUT('   ')    //falta1
                                DEVPOS(PROW(),45);DEVOUT('|')
                                IF m_nota[i,19] = 'F'
                                        DEVPOS(PROW(),47);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),47);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),47);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),47);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                ENDIF
                                DEVPOS(PROW(),53);DEVOUT('|')
*                               DEVPOS(PROW(),55);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                                DEVPOS(PROW(),55);DEVOUT('   ')    //falta2
                                DEVPOS(PROW(),59);DEVOUT('|')
                                IF m_nota[i,20] = 'F'
                                        DEVPOS(PROW(),61);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),61);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),61);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),61);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                ENDIF
                                DEVPOS(PROW(),67);DEVOUT('|')
*                               DEVPOS(PROW(),69);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                                DEVPOS(PROW(),69);DEVOUT('   ')    //falta3
                                DEVPOS(PROW(),73);DEVOUT('|')
                                IF m_nota[i,21] = 'F'
                                        DEVPOS(PROW(),75);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),75);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),75);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),75);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                ENDIF
                                DEVPOS(PROW(),81);DEVOUT('|')
*                               DEVPOS(PROW(),82);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                                DEVPOS(PROW(),82);DEVOUT('   ')   //falta4
                                DEVPOS(PROW(),87);DEVOUT('|')
                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(m_nota[i,3]+m_nota[i,5]+m_nota[i,7]+m_nota[i,9],'99.9'))   //TOTAL DAS MEDIAS
                                DEVPOS(PROW(),94);DEVOUT('|')
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),96);DEVOUT('  D ')
                                ELSE
                                        DEVPOS(PROW(),96);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                                ENDIF
                                // recuperacao anual
                                DEVPOS(PROW(),101);DEVOUT('|')
                                DEVPOS(PROW(),103);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                // media anual
                                DEVPOS(PROW(),109);DEVOUT('|')
                                DEVPOS(PROW(),111);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                // recuperacao final
                                DEVPOS(PROW(),117);DEVOUT('|')
                                DEVPOS(PROW(),119);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                // media final
                                DEVPOS(PROW(),125);DEVOUT('|')
                                DEVPOS(PROW(),127);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                // total das faltas
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                                // situacao
/*
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                                ENDIF
*/
                                IF m_nota[i,17] <> 'A'
                                        IF m_nota[i,17] = 'RA'
                                                mtipo_sit := 'RECUPERACAO'
                                        ELSEIF m_nota[i,17] = 'RF'
                                                mtipo_sit := 'RECUPERACAO FINAL'
                                        ELSE
                                                mtipo_sit := 'REPROVADO'
                                        ENDIF
                                ENDIF
                        NEXT
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',132))
                        IF mtipo_sit =='A'
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: A P R O V A D O')
                        ELSEIF mtransf = 'S'
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: TRANSFERIDO')
                        ELSE
                                DEVPOS(PROW()+2,00);DEVOUT('RESULTADO: R E P R O V A D O')
                        ENDIF
                        imprt('M','N',4)
                        DEVPOS(PROW(),00);DEVOUT(PADC(sis_cidade+', '+imp_data(mdata_sis),80))
                        DEVPOS(PROW()+6,00);DEVOUT(PADC('-----------------------------',80))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('          SECRETARIA          ',80))
                        imprt('M','N',4)
                        SETPRC(00,00)
                        EJECT
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO




































































































































































































































































