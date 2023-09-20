************************************************
* EMISSAO DE BOLETIM
************************************************

FUNCTION saf55
***************
LOCAL mprg:='SAF55'
LOCAL lci,cci,lba,cba,opcao,tela,i,mmatricula,mgstt,mtipo_con,mtransf,mobs,mobs1,;
      mobs2,mobs3

PRIVATE m_nota := {},mtipo_sit,munidade

IF ! ver_nivel(mprg,'RELATORIO EMISSAO DE BOLETIM','125')
        RETURN NIL
ENDIF

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 09
cci := 08
lba := 21
cba := 70
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mtipo_sit := SPACE(20)
        munidade := SPACE(2)
        mobs := SPACE(50)
        mobs1 := SPACE(50)
        mobs2 := SPACE(50)
        mobs3 := SPACE(50)
        mmatricula := SPACE(6)
        mtipo_con := 0
        mgstt := SPACE(4)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        @ lci+10,cci+1 TO lci+10,cba-1
        DEVPOS(lci+11,cci+1);DEVOUT('Opcao: [U1] [U2] [3U] [4U] [MU] [MA] [MF]')
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
        IF mtipo_con = 1
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                *****************
                DEVPOS(lci+3,cci+1);DEVOUT('Matricula:')
                @ lci+3,cci+12 GET mmatricula PICT '999999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mmatricula))
                        atencao('Nao foi possivel encontrar esta matricula')
                        LOOP
                ENDIF
                setcor(3)
                DEVPOS(lci+3,cci+22);DEVOUT(aluno->aluno)
                setcor(1)
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                DEVPOS(lci+6,cci+1);DEVOUT('OBS.:')
                @ lci+5,cci+10 GET munidade PICT '@!' VALID munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
                @ lci+6,cci+07 GET mobs
                @ lci+7,cci+07 GET mobs1
                @ lci+8,cci+07 GET mobs2
                @ lci+9,cci+07 GET mobs3
                READ
                IF LASTKEY() = 27
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
                * * SET PRINT TO LPT1
                imprt('M','N',1)
                DEVPOS(PROW(),42);DEVOUT(aluno->aluno)
                DEVPOS(PROW()+2,42);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),64);DEVOUT(munidade)
                DEVPOS(PROW(),85);DEVOUT(aluno->p_fone)
                DEVPOS(PROW()+2,42);DEVOUT(SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),52);DEVOUT(SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),64);DEVOUT(SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW()+3,00);DEVOUT('  ')
                ASIZE(m_nota,0)
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                boletim(aluno->matricula)
                i := 0
                FOR i = 1 TO LEN(m_nota)
                        DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,1]+'-'+m_nota[i,2])
                        IF m_nota[i,18] = 'F'
                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                        ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                        ENDIF
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta1
                        IF m_nota[i,19] = 'F'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                        ENDIF
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta2
                        IF m_nota[i,20] = 'F'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                        ENDIF
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta3
                        IF m_nota[i,21] = 'F'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                        ENDIF
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')   //falta4
                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                        ENDIF
                        // recuperacao anual
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                        // media anual
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                        // recuperacao anual
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                        // media final
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                        // total das faltas
*                       DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                        // situacao
                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                        ELSEIF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                        ENDIF
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
                IF mtipo_sit =='A'
                        DEVPOS(25,00);DEVOUT('APROVADO')
                ELSE
                        DEVPOS(25,00);DEVOUT(mtipo_sit)
                ENDIF
                DEVPOS(27,00);DEVOUT(mobs+mobs1)
                DEVPOS(28,00);DEVOUT(mobs2+mobs3)
                DEVPOS(31,10);DEVOUT(aluno->aluno)
                DEVPOS(31,54);DEVOUT(aluno->matricula)
                DEVPOS(PROW()+1,10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW(),32);DEVOUT(SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),44);DEVOUT(SUBSTR(aluno->gstt,4,1))
                imprt('M','N',4)
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
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
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                DEVPOS(lci+6,cci+1);DEVOUT('OBS.:')
                @ lci+5,cci+10 GET munidade PICT '@!' VALID munidade $ 'U1,U2,U3,U4,MU,MA,MF'
                @ lci+6,cci+07 GET mobs
                @ lci+7,cci+07 GET mobs1
                @ lci+8,cci+07 GET mobs2
                @ lci+9,cci+07 GET mobs3
                READ
                IF LASTKEY() = 27
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
                * * SET PRINT TO LPT1
                WHILE mgstt == aluno->gstt .AND. ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        imprt('M','N',1)
                        DEVPOS(PROW(),42);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+2,42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),64);DEVOUT(munidade)
                        DEVPOS(PROW(),85);DEVOUT(aluno->p_fone)
                        DEVPOS(PROW()+2,42);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),52);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),64);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW()+3,00);DEVOUT('  ')
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,1]+'-'+m_nota[i,2])
                                IF m_nota[i,18] = 'F'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta1
                                IF m_nota[i,19] = 'F'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta2
                                IF m_nota[i,20] = 'F'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta3
                                IF m_nota[i,21] = 'F'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')   //falta4
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                                ENDIF
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                // media anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                // media final
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                // total das faltas
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                                // situacao
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                                ENDIF
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
                        IF mtipo_sit =='A'
                                DEVPOS(25,00);DEVOUT('APROVADO')
                        ELSE
                                DEVPOS(25,00);DEVOUT(mtipo_sit)
                        ENDIF
                        DEVPOS(27,00);DEVOUT(mobs+mobs1)
                        DEVPOS(28,00);DEVOUT(mobs2+mobs3)
                        DEVPOS(31,05);DEVOUT(aluno->aluno)
                        DEVPOS(31,54);DEVOUT(aluno->matricula)
                        DEVPOS(PROW()+1,10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),32);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),44);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        imprt('M','N',4)
                        SETPRC(00,00)
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSE
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                DEVPOS(lci+6,cci+1);DEVOUT('OBS.:')
                @ lci+5,cci+10 GET munidade PICT '@!' VALID munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
                @ lci+6,cci+07 GET mobs
                @ lci+7,cci+07 GET mobs1
                @ lci+8,cci+07 GET mobs2
                @ lci+9,cci+07 GET mobs3
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
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
                * * SET PRINT TO LPT1
                WHILE ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        imprt('M','N',1)
                        DEVPOS(PROW(),42);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+2,42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),85);DEVOUT(aluno->p_fone)
                        DEVPOS(PROW()+2,42);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),52);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),64);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW()+3,00);DEVOUT('  ')
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,1]+'-'+m_nota[i,2])
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta1
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,6],'999'))    //falta2
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta2
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,8],'999'))    //falta3
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')    //falta3
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,10],'999'))   //falta4
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')   //falta4
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media unidade
                                ENDIF
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                // media anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                // media final
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                // total das faltas
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4]+m_nota[i,6]+m_nota[i,8]+m_nota[i,10],'999'))
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('   ')
                                // situacao
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                                ENDIF
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
                        IF mtipo_sit =='A'
                                DEVPOS(25,00);DEVOUT('APROVADO')
                        ELSE
                                DEVPOS(25,00);DEVOUT(mtipo_sit)
                        ENDIF
                        DEVPOS(27,00);DEVOUT(mobs+mobs1)
                        DEVPOS(28,00);DEVOUT(mobs2+mobs3)
                        DEVPOS(31,10);DEVOUT(aluno->aluno)
                        DEVPOS(31,54);DEVOUT(aluno->matricula)
                        DEVPOS(PROW()+1,10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),20);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),32);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),44);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        imprt('M','N',4)
                        SETPRC(00,00)
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO

