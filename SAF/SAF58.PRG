************************************************
* EMISSAO DO MAPA DAS MEDIAS POR DISCIPLINAS
************************************************

FUNCTION saf58
***************
LOCAL mprg:='SAF58'
LOCAL lci,cci,lba,cba,opcao,tela,i,mgstt,mtipo_con,mtransf,mtraco,mpoint,mtp_sit

PRIVATE m_nota := {},mu,ma,mf,msituacao,munidade

IF ! ver_nivel(mprg,'RELATORIO DO MAPA DAS MEDIAS P/DISCIPLINA','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 09
cci := 08
lba := 15
cba := 64
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mtp_sit := 'GE'
        munidade := SPACE(2)
        mtipo_con := 0
        mgstt := SPACE(4)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Opcao: [U1] [U2] [3U] [4U] [MU] [RA] [MA] [RF] [MF]')
        setcor(1)
        @ lci+1,cci+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim por Grau, Serie, Turma e turno'
        @ lci+1,COL()+2 PROMPT '    Geral   ' MESSAGE 'Emissao de Boletim em GERAL'
        SET INTEN ON
        MENU TO mtipo_con

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF mtipo_con = 1
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
                mpoint := RECNO()
                DEVPOS(lci+3,cci+14);DEVOUT('Unidade:')
                DEVPOS(lci+3,cci+26);DEVOUT('Tipo situacao:')
                @ lci+3,cci+23 GET munidade PICT '@!' WHEN men_get(lci+4,cci+23,'Informe o Tipo de Unidade que deseja [U1]1a.unidade [U2]2a.unidade [U3]3a.unidade [U4]4a.unidade [MU]media unidade [RA]recup.anual [MA]media anual [RF]recup.final [MF]media final') VALID lim_get() .AND. munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
                @ lci+3,cci+41 GET mtp_sit PICT '@!' WHEN men_get(lci+4,cci+41,'Informe o Tipo de Situacao que deseja [GE]geral [AP]aprovados [RE]recuperacao') VALID lim_get() .AND. mtp_sit $ 'GE,AP,RE'
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
                mpag := 1
                ASIZE(m_nota,0)
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                boletim(aluno->matricula)
                i := 0
                FOR i = 1 TO LEN(m_nota)
                        IF LASTKEY() = 27
                                EXIT
                        ENDIF
                        *****************
                        SELE('aluno');ORDSETFOCUS(2)
                        GO mpoint
                        *****************
                        mtit := 'MAPA DE MEDIAS POR DISCIPLINAS: ****  '+m_nota[i,1]+'-'+RTRIM(m_nota[i,2])+'  ****'
                        IF mtp_sit = 'GE'
                                mtipo := 'GRAU: '+SUBSTR(mgstt,1,1)+' SERIE: '+SUBSTR(mgstt,2,1)+' TURMA: '+SUBSTR(mgstt,3,1)+' TURNO: '+SUBSTR(mgstt,4,1)+'  UNIDADE: '+munidade+'     ** GERAL **'
                        ELSEIF mtp_sit = 'AP'
                                mtipo := 'GRAU: '+SUBSTR(mgstt,1,1)+' SERIE: '+SUBSTR(mgstt,2,1)+' TURMA: '+SUBSTR(mgstt,3,1)+' TURNO: '+SUBSTR(mgstt,4,1)+'  UNIDADE: '+munidade+'   ** APROVADOS **'
                        ELSE
                                mtipo := 'GRAU: '+SUBSTR(mgstt,1,1)+' SERIE: '+SUBSTR(mgstt,2,1)+' TURMA: '+SUBSTR(mgstt,3,1)+' TURNO: '+SUBSTR(mgstt,4,1)+'  UNIDADE: '+munidade+'  ** RECUPERACAO **'
                        ENDIF
                        cabecalho(mpag,mtit,mtipo,,)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT('Matricula')
                        DEVPOS(PROW(),12);DEVOUT('Nome do Aluno')
                        DEVPOS(PROW(),63);DEVOUT(' U1 ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' U2 ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' U3 ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' U4 ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' MU ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' RA ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' MA ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' RF ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' MF ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Sit.')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('N O T A')
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                        imprt('M','C')
                        WHILE mgstt == aluno->gstt .AND. ! EOF()
                                INKEY(.1)
                                IF LASTKEY() = 27
                                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                        EXIT
                                ENDIF
                                DEVPOS(PROW(),54);DEVOUT(' ')
                                IF ! ver_notas(mgstt,m_nota[i,1],aluno->matricula)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF mtp_sit == 'RE'
                                        IF SUBSTR(msituacao,1,1) == 'A'
                                                SKIP
                                                LOOP
                                        ENDIF
                                ELSEIF mtp_sit == 'AP'
                                        IF SUBSTR(msituacao,1,1) == 'R'
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtransf := SPACE(1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        mtransf := 'S'
                                ENDIF
                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(nota->media_1,'99.9'))  //media1
                                ENDIF
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(nota->media_2,'99.9'))   //media2
                                ENDIF
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(nota->media_3,'99.9'))   //media3
                                ENDIF
                                IF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('tran')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(nota->media_4,'99.9'))   //media4
                                ENDIF
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  D ')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(mu,'99.9'))  //media unidade
                                ENDIF
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(nota->nt_recan,'99.9')))
                                // media anual
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(ma,'99.9')))
                                // recuperacao anual
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(nota->nt_prfi,'99.9')))
                                // media final
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(mf,'99.9')))
                                // total das faltas
                                // situacao
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(msituacao)
                                ENDIF
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('________')
                                SKIP
                        ENDDO
                        mpag++
                        EJECT
                NEXT
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
/*
        ELSE
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                @ lci+5,cci+10 GET munidade PICT '@!' VALID munidade $ 'I  ,II ,III,IV '
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
                        DEVPOS(PROW(),85);DEVOUT(aluno->p_fone1)
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
                        NEXT
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
*/
        ENDIF
ENDDO
**************************** F I M **************************
