************************************************
* RELATORIO RESUMO DE BOLETINS
************************************************

FUNCTION saf514
***************
LOCAL mprg:='SAF514'
LOCAL lci,cci,lba,cba,opcao,tela,i,mmatricula,mgstt,mtipo_con,mtransf,;
      maluno,msele,morde,mpoint,mpag:=1,mtit

PRIVATE m_nota := {},mtipo_sit,munidade,mcont_rec,ma,mu,mf,msituacao
PRIVATE mtipo_imp:='M'

IF ! ver_nivel(mprg,'RELATORIO EMISSAO DE BOLETIM','125')
        RETURN NIL
ENDIF
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tab');RETURN NIL;ENDIF
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
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mtipo_con := 0
        mgstt := SPACE(4)
        mcont_rec := 0
        mtit := 'Resumo de BOLETINS do GSTT: '+mgstt
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Emissao de Boletim ','*')
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        @ lci+10,cci+1 TO lci+10,cba-1
        DEVPOS(lci+11,cci+1);DEVOUT('Opcao: [U1] [U2] [3U] [4U] [MU] [MA] [MF]')
        setcor(1)
        @ lci+1,cci+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim por Grau, Serie, Turma e turno'
        @ lci+1,COL()+4 PROMPT ' Individual ' MESSAGE 'Emissao de Boletim INDIVIDUAL'
        SET INTEN ON
        MENU TO mtipo_con

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF mtipo_con = 1
                *****************
*               SELE('aluno');ORDSETFOCUS(2)
                SELE('aluno');ORDSETFOCUS(6)
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
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                IF ! imp_arq('RES_BOLE.REL')
                        LOOP
                ENDIF
                mpag := 1
                IF munidade = 'U1' .OR. munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4'
                        cabecalho(mpag,mtit)
                        WHILE mgstt == aluno->gstt .AND. ! EOF()
                                IF PROW() >= 55
                                        mpag ++
                                        EJECT
                                        cabecalho(mpag,mtit)
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                        DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                        DEVPOS(PROW(),34);DEVOUT('  U1')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                ENDIF
                                msele := SELE()
                                morde := INDEXORD()
                                mpoint := RECNO()
                                ****************
                                SELE('nota')
                                ORDSETFOCUS(1)
                                GO TOP
                                ****************
                                SEEK aluno->matricula
                                IF ! FOUND()
                                        ****************
                                        SELE(msele)
                                        ORDSETFOCUS(morde)
                                        GO mpoint
                                        ****************
                                        SKIP
                                        LOOP
                                ENDIF
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(REPLI('=',80))
                                DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+aluno->matricula+'-'+aluno->aluno)
                                DEVPOS(PROW()+1,00);DEVOUT('Unidade: '+munidade)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('GSTT: '+aluno->gstt)
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                DEVPOS(PROW(),34);DEVOUT('  U1')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                WHILE nota->matricula = aluno->matricula .AND. ! EOF()
                                        IF PROW() >= 55
                                                mpag ++
                                                EJECT
                                                cabecalho(mpag,mtit)
                                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                                DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                                DEVPOS(PROW(),34);DEVOUT('  U1')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                        ENDIF
                                        ver_notas(mgstt,nota->cod_disc,nota->matricula)
                                        DEVPOS(PROW()+1,00);DEVOUT(nota->cod_disc+'-'+ver_disc(nota->cod_disc))
                                        IF munidade = 'U1' .OR. munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4';
                                           .OR. munidade = 'MU' .OR. munidade = 'MA' .OR. munidade = 'MF'
                                                IF ! EMPTY(aluno->dt_tranf)
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                                ELSEIF ! EMPTY(nota->ftpr_1)
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  '+ftpr_1+' ')
                                                ELSEIF disc->tipo = 'C'
                                                        IF nota->media_1 >= 6
                                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  AS')
                                                        ELSE
                                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  AI')
                                                        ENDIF
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(nota->media_1,'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                        IF munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4';
                                           .OR. munidade = 'MU' .OR. munidade = 'MA' .OR. munidade = 'MF'
                                              IF ! EMPTY(aluno->dt_tranf)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                                ELSEIF ! EMPTY(nota->ftpr_2)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_2+' ')
                                                ELSEIF disc->tipo = 'C'
                                                        IF nota->media_2 >= 6
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                        ELSE
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                        ENDIF
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_2,'99.9'))  //media2
                                                ENDIF
                                        ENDIF
                                        IF munidade = 'U3' .OR. munidade = 'U4';
                                           .OR. munidade = 'MU' .OR. munidade = 'MA' .OR. munidade = 'MF'
                                              IF ! EMPTY(aluno->dt_tranf)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                                ELSEIF ! EMPTY(nota->ftpr_3)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_3+' ')
                                                ELSEIF disc->tipo = 'C'
                                                        IF nota->media_3 >= 6
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                        ELSE
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                        ENDIF
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_3,'99.9'))  //media3
                                                ENDIF
                                        ENDIF
                                        IF munidade = 'U4' .OR. munidade = 'MU' .OR. munidade = 'MA' .OR. munidade = 'MF'
                                              IF ! EMPTY(aluno->dt_tranf)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                                ELSEIF ! EMPTY(nota->ftpr_4)
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_4+' ')
                                                ELSEIF disc->tipo = 'C'
                                                        IF nota->media_4 >= 6
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                        ELSE
                                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                        ENDIF
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_4,'99.9'))  //media4
                                                ENDIF
                                        ENDIF
                                        IF munidade = 'MU' .OR. munidade = 'MA' .OR. munidade = 'MF'
                                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*'
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mu,'99.9'))  //media unidade
                                                ENDIF
                                        ENDIF
                                        IF munidade = 'MA' .OR. munidade = 'MF'
                                                // recuperacao anual
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(nota->nt_recan),SPACE(4),TRANSFORM(nota->nt_recan,'99.9')))
                                                // media anual
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(ma),SPACE(4),TRANSFORM(ma,'99.9')))
                                        ENDIF
                                        IF munidade = 'MF'
                                                // recuperacao anual
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(nota->nt_prfi),SPACE(4),TRANSFORM(nota->nt_prfi,'99.9')))
                                                // media final
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(mf),SPACE(4),TRANSFORM(mf,'99.9')))
                                        ENDIF
                                        SKIP
                                ENDDO
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                imprt('M','N')
                                ****************
                                SELE(msele)
                                ORDSETFOCUS(morde)
                                GO mpoint
                                ****************
                                SKIP
                        ENDDO
                ELSE
                        cabecalho(mpag,mtit)
                        WHILE mgstt == aluno->gstt .AND. ! EOF()
                                IF PROW() >= 55
                                        mpag ++
                                        EJECT
                                        cabecalho(mpag,mtit)
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                        DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                        DEVPOS(PROW(),34);DEVOUT('  U1')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                ENDIF
                                INKEY(.1)
                                msele := SELE()
                                morde := INDEXORD()
                                mpoint := RECNO()
                                mcont_rec := 0
                                IF LASTKEY() = 27
                                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                        EXIT
                                ENDIF
                                ASIZE(m_nota,0)
                                ****************
                                SELE('nota')
                                ORDSETFOCUS(1)
                                GO TOP
                                ****************
                                SEEK aluno->matricula
                                IF ! FOUND()
                                        ****************
                                        SELE(msele)
                                        ORDSETFOCUS(morde)
                                        GO mpoint
                                        ****************
                                        SKIP
                                        LOOP
                                ENDIF
                                ****************
                                SELE(msele)
                                ORDSETFOCUS(morde)
                                GO mpoint
                                ****************
                                mtransf := SPACE(1)
*                               IF ! EMPTY(aluno->dt_tranf)
*                                       mtransf := 'S'
*                               ENDIF
                                boletim(aluno->matricula)
*                               IF (mtipo_sit ='A' .OR. mtransf = 'S')  .AND. (munidade = 'MA' .OR. munidade = 'MF')
*                                       SKIP
*                                       LOOP
*                               ENDIF
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(REPLI('=',80))
                                DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+aluno->matricula+'-'+aluno->aluno)
                                DEVPOS(PROW()+1,00);DEVOUT('Unidade: '+munidade)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('GSTT: '+aluno->gstt)
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                DEVPOS(PROW(),34);DEVOUT('  U1')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                i := 0
                                FOR i = 1 TO LEN(m_nota)
                                        IF PROW() >= 55
                                                mpag ++
                                                EJECT
                                                cabecalho(mpag,mtit)
                                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('=',80))
                                                DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                                                DEVPOS(PROW(),34);DEVOUT('  U1')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                                        ENDIF
                                        DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,1]+'-'+m_nota[i,2])
                                        IF m_nota[i,18] = 'F'
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  F ')
                                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                        ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,3] >= 6
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                        ENDIF
                                        IF m_nota[i,19] = 'F'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                        ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,5] >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                        ENDIF
                                        IF m_nota[i,20] = 'F'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                        ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,7] >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                        ENDIF
                                        IF m_nota[i,21] = 'F'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  F ')
                                        ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                        ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,9] >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                        ENDIF
                                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,11] >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
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
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
/*
                                IF mtipo_sit ='A'
                                        DEVPOS(25,00);DEVOUT('APROVADO')
                                ELSE
                                        DEVPOS(25,00);DEVOUT(mtipo_sit)
                                ENDIF
*/
                                imprt('M','N')
                                SKIP
                        ENDDO
                ENDIF
                imprt('M','N')
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSEIF mtipo_con = 2
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
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                @ lci+5,cci+10 GET munidade PICT '@!' VALID munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
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
*               SET PRINT TO 'LPT1'
                IF munidade = 'U1' .OR. munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4'
                        imprt('M','N')
                        DEVPOS(PROW()+1,42);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+2,42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),64);DEVOUT(munidade)
                        DEVPOS(PROW(),85);DEVOUT(aluno->p_fone)
                        DEVPOS(PROW()+2,42);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),52);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),64);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                        DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                        DEVPOS(PROW(),34);DEVOUT('  U1')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                        ****************
                        SELE('nota')
                        ORDSETFOCUS(1)
                        GO TOP
                        ****************
                        SEEK mmatricula
*                       IF ! FOUND()
*                               SETPRC(00,00)
*                               SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
*                               LOOP
*                       ENDIF
                        WHILE nota->matricula = mmatricula .AND. ! EOF()
                                DEVPOS(PROW()+1,00);DEVOUT(nota->cod_disc+'-'+ver_disc(nota->cod_disc))
                                IF munidade = 'U1' .OR. munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4'
                                      IF ! EMPTY(aluno->dt_tranf)
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                        ELSEIF ! EMPTY(nota->ftpr_1)
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  '+ftpr_1+' ')
                                        ELSEIF disc->tipo = 'C'
                                                IF nota->media_1 >= 6
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(nota->media_1,'99.9'))  //media1
                                        ENDIF
                                ENDIF
                                IF munidade = 'U2' .OR. munidade = 'U3' .OR. munidade = 'U4'
                                      IF ! EMPTY(aluno->dt_tranf)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF ! EMPTY(nota->ftpr_2)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_2+' ')
                                        ELSEIF disc->tipo = 'C'
                                                IF nota->media_2 >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_2,'99.9'))  //media2
                                        ENDIF
                                ENDIF
                                IF munidade = 'U3' .OR. munidade = 'U4'
                                      IF ! EMPTY(aluno->dt_tranf)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF ! EMPTY(nota->ftpr_3)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_3+' ')
                                        ELSEIF disc->tipo = 'C'
                                                IF nota->media_3 >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_3,'99.9'))  //media3
                                        ENDIF
                                ENDIF
                                IF munidade = 'U4'
                                      IF ! EMPTY(aluno->dt_tranf)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                        ELSEIF ! EMPTY(nota->ftpr_4)
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+ftpr_4+' ')
                                        ELSEIF disc->tipo = 'C'
                                                IF nota->media_4 >= 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                                ENDIF
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(nota->media_4,'99.9'))  //media4
                                        ENDIF
                                ENDIF
                                SKIP
                        ENDDO
                        DEVPOS(PROW()+2,00);DEVOUT('Responsavel: '+aluno->responsa)

                ELSE
                        DEVPOS(PROW()+1,42);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+2,42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),64);DEVOUT(munidade)
                        DEVPOS(PROW(),85);DEVOUT(aluno->p_fone)
                        DEVPOS(PROW()+2,42);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),52);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),64);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),76);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                        DEVPOS(PROW()+1,00);DEVOUT('DISCIPLINAS')
                        DEVPOS(PROW(),34);DEVOUT('  U1')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U2')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U3')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  U4')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.UN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.AN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.AN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('R.FN')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('M.FN')
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                DEVPOS(PROW()+1,00);DEVOUT(m_nota[i,1]+'-'+m_nota[i,2])
                                IF ! EMPTY(m_nota[i,22]) .AND. EMPTY(m_nota[i,3])
*                               IF m_nota[i,18] = 'F'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  '+m_nota[i,22]+' ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT('tran')
                                ELSEIF m_nota[i,26] = 'C'
                                        IF m_nota[i,3] >= 6
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+11);DEVOUT('  AI')
                                        ENDIF
                                ELSE
                                        DEVPOS(PROW(),PCOL()+11);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                ENDIF
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,4],'999'))    //falta1
                                IF ! EMPTY(m_nota[i,23]) .AND. EMPTY(m_nota[i,5])
*                               IF m_nota[i,19] = 'F'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+m_nota[i,23]+' ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSEIF m_nota[i,26] = 'C'
                                        IF m_nota[i,5] >= 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                        ENDIF
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))   //media2
                                ENDIF
                                IF ! EMPTY(m_nota[i,24]) .AND. EMPTY(m_nota[i,7])
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+m_nota[i,24]+' ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSEIF m_nota[i,26] = 'C'
                                        IF m_nota[i,7] >= 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                        ENDIF
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))   //media3
                                ENDIF
                                IF ! EMPTY(m_nota[i,25]) .AND. EMPTY(m_nota[i,9])
*                               IF m_nota[i,21] = 'F'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  '+m_nota[i,25]+' ')
                                ELSEIF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('tran')
                                ELSEIF m_nota[i,26] = 'C'
                                        IF m_nota[i,9] >= 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                        ENDIF
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))   //media4
                                ENDIF
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF m_nota[i,26] = 'C'
                                        IF m_nota[i,11] >= 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('  AI')
                                        ENDIF
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
                                // situacao
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('  D ')
                                ELSEIF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('TRAN')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(m_nota[i,16])
                                ENDIF
                                IF m_nota[i,17] <> 'A'
                                        IF mcont_rec > 4
                                                mtipo_sit := 'REPROVADO'
                                        ELSEIF m_nota[i,17] = 'RA'
                                                mtipo_sit := 'RECUPERACAO'
                                        ELSEIF m_nota[i,17] = 'RF'
                                                mtipo_sit := 'RECUPERACAO FINAL'
                                        ELSE
                                                mtipo_sit := 'REPROVADO'
                                        ENDIF
                                ENDIF
                        NEXT
                        IF mtipo_sit = 'A' .AND. mcont_rec <= 4
                                DEVPOS(PROW()+2,00);DEVOUT('APROVADO')
                        ELSE
                                DEVPOS(PROW()+2,00);DEVOUT(mtipo_sit)
                        ENDIF
                        DEVPOS(PROW()+2,00);DEVOUT('Responsavel: '+aluno->responsa)
                ENDIF
                EJECT
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO

