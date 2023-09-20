************************************************
* EMISSAO DO MAPA NOTAS POR TURMA/UNIDADE
************************************************

FUNCTION saf503
***************
LOCAL mprg:='SAF503'
LOCAL lci,cci,lba,cba,opcao,tela,i,mgstt,mtipo_con,mtransf,mtraco,msit_nota,;
      mimp_nota:='N'
PRIVATE m_nota := {},munidade,mcont_rec,mtipo_sit:=SPACE(20)
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35),mqtd_dis:=0,mqtd_rec:=0

IF ! ver_nivel(mprg,'RELATORIO DO MAPA NOTAS P/TURMA/UNIDADE','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',264)

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
        msit_nota := ' '
        munidade := SPACE(2)
        mtipo_con := 1
        mgstt := SPACE(4)
        mcont_rec := 0
        mqtd_dis := 0
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Mapa de Notas ','*')
        DEVPOS(lci+5,cci+1);DEVOUT('Opcao: [U1] [U2] [3U] [4U] [MU] [RA] [MA] [RF] [MF]')
        setcor(1)
        IF mtipo_con = 1
                *****************
*               SELE('aluno');ORDSETFOCUS(2)
                SELE('aluno');ORDSETFOCUS(6)
                GO TOP
                *****************
                DEVPOS(lci+1,cci+1);DEVOUT('GSTT...:')
                DEVPOS(lci+2,cci+1);DEVOUT('Unidade:')
                DEVPOS(lci+3,cci+1);DEVOUT('Quantidade de Disciplina que nao atingiu a media:')
                @ lci+1,cci+10 GET mgstt PICT '@!'
                @ lci+2,cci+10 GET munidade PICT '@!' WHEN men_get(lci+3,cci+10,'Informe o Tipo de Unidade que deseja [U1]1a.unidade [U2]2a.unidade [U3]3a.unidade [U4]4a.unidade [MU]media unidade [RA]recup.anual [MA]media anual [RF]recup.final [MF]media final') VALID lim_get() .AND. munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
                @ lci+3,cci+50 GET mqtd_dis PICT '99'
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                IF ! aluno->(DBSEEK(mgstt))
                        atencao('Nao foi possivel encontrar esta TURMA')
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                IF ! imp_arq('MAP_NOTAS.REL')
                        LOOP
                ENDIF
                mpag := 1
                mtit := 'M A P A S   D E   N O T A S'
                IF ! EMPTY(mqtd_dis)
                        mtit := mtit + '  **** Alunos em Recuperacao em '+STRZERO(mqtd_dis,2)+' Disciplina ****'
                ENDIF
                mtipo := 'GRAU: '+SUBSTR(mgstt,1,1)+' SERIE: '+SUBSTR(mgstt,2,1)+' TURMA: '+SUBSTR(mgstt,3,1)+' TURNO: '+SUBSTR(mgstt,4,1)+'  UNIDADE: '+munidade
                cabecalho(mpag,mtit,mtipo,,132)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Matricula')
                DEVPOS(PROW(),12);DEVOUT('Nome do Aluno')
                ASIZE(m_nota,0)
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                boletim(aluno->matricula,munidade)

                IF ! EMPTY(mqtd_dis) .AND. mqtd_rec = mqtd_dis
                        WHILE LEN(m_nota) = 0
                                SKIP
                                mtransf := SPACE(1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        mtransf := 'S'
                                ENDIF
                                boletim(aluno->matricula,munidade)
                        ENDDO
                        msit_nota := ' '
                        i := 0
                        DEVPOS(PROW(),55);DEVOUT(SUBSTR(m_nota[1,2],1,8))
                        FOR i = 2 TO LEN(m_nota)
                                DEVPOS(PROW(),PCOL()+3);DEVOUT(SUBSTR(m_nota[i,2],1,8))
                        NEXT
                        mimp_nota:='S'
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        DEVPOS(PROW(),54);DEVOUT(' ')
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                IF munidade = 'U1'
                                        IF EMPTY(m_nota[i,3]) .AND. ! EMPTY(m_nota[i,22])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,22]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOl()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,3] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,3] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U2'
                                        IF EMPTY(m_nota[i,5]) .AND. ! EMPTY(m_nota[i,23])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,23]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,5] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,5] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U3'
                                        IF EMPTY(m_nota[i,7]) .AND. ! EMPTY(m_nota[i,24])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,24]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,7] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,7] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U4'
                                        IF EMPTY(m_nota[i,9]) .AND. ! EMPTY(m_nota[i,25])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,25]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,9] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,9] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  D ')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MU'          //.OR. m_nota[i,11] >= 6
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  D ')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,11] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,11] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        msit_nota := '*'
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'RA'
                                        // recuperacao anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,12] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,12] < 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MA'
                                        // media anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,13] < 6 .AND. m_nota[i,11] < 7
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,13] < 6 .AND. m_nota[i,11] < 7
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'RF'
                                        // recuperacao anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,14] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,14] < 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MF'
                                        // media final
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,15] < 6     //.AND. m_nota[i,11] < 7
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,15] < 6         //.AND. m_nota[i,11] < 7
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ENDIF
                                DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                        NEXT
                        IF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('TRANSFERIDO')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MU'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MU'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('RECUPERACAO')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MA'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MA'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('RECUPERACAO FINAL')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MF'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MF'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('REPROVADO')
                        ENDIF
                        SKIP
                ENDIF
                WHILE mgstt = aluno->gstt .AND. ! EOF()
                        mcont_rec := 0
                        ASIZE(m_nota,0)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        boletim(aluno->matricula,munidade)

                        WHILE LEN(m_nota) = 0
                                SKIP
                                boletim(aluno->matricula,munidade)
                        ENDDO
                        IF ! EMPTY(mqtd_dis) .AND. mqtd_rec <> mqtd_dis
                                SKIP
                                LOOP
                        ENDIF
                        IF mimp_nota = 'N'
                                i := 0
                                DEVPOS(PROW(),55);DEVOUT(SUBSTR(m_nota[1,2],1,8))
                                FOR i = 2 TO LEN(m_nota)
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(SUBSTR(m_nota[i,2],1,8))
                                NEXT
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                mimp_nota:='S'
                        ENDIF
                        msit_nota := ' '
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                        mtransf := SPACE(1)
                        IF ! EMPTY(aluno->dt_tranf)
                                mtransf := 'S'
                        ENDIF
                        i := 0
                        DEVPOS(PROW(),54);DEVOUT(' ')
                        FOR i = 1 TO LEN(m_nota)
                                IF munidade = 'U1'
                                        IF EMPTY(m_nota[i,3]) .AND. ! EMPTY(m_nota[i,22])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,22]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOl()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,3] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,3] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,3],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U2'
                                        IF EMPTY(m_nota[i,5]) .AND. ! EMPTY(m_nota[i,23])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,23]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,5] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,5] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,5],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U3'
                                        IF EMPTY(m_nota[i,7]) .AND. ! EMPTY(m_nota[i,24])
*                                       IF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,24]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,7] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,7] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,7],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF munidade = 'U4'
                                        IF EMPTY(m_nota[i,9]) .AND. ! EMPTY(m_nota[i,25])
        *                               IF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  '+m_nota[i,25]+' ')
                                        ELSEIF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,9] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,9] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,9],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  D ')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MU'          //.OR. m_nota[i,11] >= 6
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('  D ')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,11] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSE
                                                IF m_nota[i,11] < 6
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                        msit_nota := '*'
                                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media1
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(TRANSFORM(m_nota[i,11],'99.9'))  //media1
                                                ENDIF
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'RA'
                                        // recuperacao anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,12] < 6
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                        msit_nota := '*'
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,12] < 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MA'
                                        // media anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,13] < 6 .AND. m_nota[i,11] < 7
                                                        msit_nota := '*'
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,13] < 6 .AND. m_nota[i,11] < 7
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'RF'
                                        // recuperacao anual
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,14] < 6
                                                        msit_nota := '*'
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,14] < 6
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ELSEIF munidade = 'MF'
                                        // media final
                                        IF mtransf = 'S'
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('tran')
                                        ELSEIF m_nota[i,26] = 'C'
                                                IF m_nota[i,15] < 6     //.AND. m_nota[i,11] < 7
                                                        msit_nota := '*'
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AI')
                                                ELSE
                                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('  AS')
                                                ENDIF
                                        ELSEIF m_nota[i,15] < 6         //.AND. m_nota[i,11] < 7
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                                msit_nota := '*'
                                                DEVPOS(PROW(),PCOL()+1);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                        ENDIF
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                                        LOOP
                                ENDIF
                                DEVPOS(PROW(),PCOL()+3);DEVOUT(' ')
                        NEXT
                        IF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('TRANSFERIDO')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MU'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MU'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('RECUPERACAO')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MA'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MA'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('RECUPERACAO FINAL')
                        ELSEIF msit_nota =' ' .AND. munidade = 'MF'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('APROVADO')
                        ELSEIF msit_nota ='*' .AND. munidade = 'MF'
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('REPROVADO')
                        ENDIF
                        IF PROW() >= 58
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo,,132)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Matricula')
                                DEVPOS(PROW(),12);DEVOUT('Nome do Aluno')
                                i := 0
                                DEVPOS(PROW(),55);DEVOUT(SUBSTR(m_nota[1,2],1,8))
                                FOR i = 2 TO LEN(m_nota)
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT(SUBSTR(m_nota[i,2],1,8))
                                NEXT
                        ENDIF
                        SKIP
                ENDDO
                EJECT
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                IF mimp_tipo = 2
                        lertexto('MAP_NOTAS.REL')
                ENDIF
                LOOP
        ELSE
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                DEVPOS(lci+5,cci+1);DEVOUT('Unidade:')
                @ lci+5,cci+10 GET munidade PICT '@!' WHEN men_get(lci+4,cci+23,'Informe o Tipo de Unidade que deseja [U1]1a.unidade [U2]2a.unidade [U3]3a.unidade [U4]4a.unidade [MU]media unidade [RA]recup.anual [MA]media anual [RF]recup.final [MF]media final') VALID lim_get() .AND. munidade $ 'U1,U2,U3,U4,MU,RA,MA,RF,MF'
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
*               * SET PRINT TO LPT1
                WHILE ! EOF()
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
        ENDIF
ENDDO
**************************** F I M **************************
