************************************************
* EMISSAO DO ATA RESULTADOS FINAIS
************************************************

FUNCTION saf513
***************
LOCAL mprg:='SAF513'
LOCAL lci,cci,lba,cba,opcao,tela,i,mgstt,mtipo_con,mtransf,mtraco,msituacao,m_desc:={}
PRIVATE m_nota:={},munidade,mcont_rec,mtipo_imp:='M'

IF ! ver_nivel(mprg,'RELATORIO DA ATA RESULTADO FINAL','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',132)

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 09
cci := 08
lba := 11
cba := 28
tela := SAVESCREEN(00,00,24,79)
WHILE .T.
        msituacao := ' '
        munidade := SPACE(2)
        mtipo_con := 0
        mgstt := SPACE(4)
        mcont_rec := 0
        @ 23,00 CLEAR TO 23,79
        setcor(1)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Mapa de Notas ','*')
        ASIZE(m_nota,0)
        ASIZE(m_desc,0)
        DEVPOS(lci+1,cci+1);DEVOUT('GSTT:')
        @ lci+1,COL()+2 GET mgstt PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        *****************
        SELE('aluno');ORDSETFOCUS(2)
        GO TOP
        *****************
        IF ! aluno->(DBSEEK(mgstt))
                atencao('Nao foi possivel encontrar esta TURMA')
                LOOP
        ENDIF
        munidade := 'MF'
/*
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
*/
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('ATA_RESU.REL')
                LOOP
        ENDIF
*       SET DEVI TO PRINT
*       SET PRINT TO 'LPT1'
        mpag := 1
        imprt('M','N',1)
        IF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12' .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (1a. a 4a.) Serie',132))
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (1a. a 4a.) Serie',132))
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Fundamental deste estabelecimento, com os seguintes resultados:')
        ELSEIF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16' .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18'
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (5a. a 8a.) Serie',132))
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (5a. a 8a.) Serie',132))
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Fundamental deste estabelecimento, com os seguintes resultados:')
        ELSE
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO MEDIO (1o. a 3o.) Ano',132))
                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO MEDIO (1o. a 3o.) Ano',132))
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Medio deste estabelecimento, com os  seguintes resultados:')
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        imprt('M','C',1)
*       DEVPOS(PROW(),00);DEVOUT('Matricula')
*       DEVPOS(PROW(),12);DEVOUT('Nome do Aluno')
*       DEVPOS(PROW(),00);DEVOUT('Matricula')
        DEVPOS(PROW(),00);DEVOUT('ATIVIDADES,AREAS DE ESTUDO OU DISCIPLINAS              |')
        DEVPOS(PROW(),00);DEVOUT('ATIVIDADES,AREAS DE ESTUDO OU DISCIPLINAS              |')
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
        i := 0
        DEVPOS(PROW(),PCOL()+1);DEVOUT(SUBSTR(m_nota[1,2],1,10))
        FOR i = 2 TO LEN(m_nota)
                DEVPOS(PROW(),PCOL()+2);DEVOUT(SUBSTR(m_nota[i,2],1,10))
        NEXT
        DEVPOS(PROW(),PCOL()+2);DEVOUT('RESULT.')
        i := 0
        DEVPOS(PROW()+1,0);DEVOUT('Nome do Alunos                                          ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(SUBSTR(m_nota[1,2],11))
        FOR i = 2 TO LEN(m_nota)
                DEVPOS(PROW(),PCOL()+2);DEVOUT(SUBSTR(m_nota[i,2],11))
        NEXT
        DEVPOS(PROW(),PCOL()+2);DEVOUT('FINAIS')
        IF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16' .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18' .OR.;
           SUBSTR(mgstt,1,2) = '21' .OR. SUBSTR(mgstt,1,2) = '22' .OR. SUBSTR(mgstt,1,2) = '23'
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(REPLI('-',132))
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('TOTAIS DE HORAS DADAS:                                  ')
                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(ver_ch(mgstt,m_nota[1,1]),'9999'))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                i := 0
                FOR i = 2 TO LEN(m_nota)
                        DEVPOS(PROW(),PCOL()+5);DEVOUT(TRANSFORM(ver_ch(mgstt,m_nota[i,1]),'9999'))
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                NEXT
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        imprt('M','C',1)
*       DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
*       DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
        DEVPOS(PROW(),00);DEVOUT(aluno->aluno1)
        mtransf := SPACE(1)
        IF ! EMPTY(aluno->dt_tranf)
                mtransf := 'S'
        ENDIF
        msituacao := ' '
        i := 0
        FOR i = 1 TO LEN(m_nota)
                IF (m_nota[i,22] = 'D' .AND. m_nota[i,23] .AND. m_nota[i,24] .AND. m_nota[i,25]) .OR. ;
                   (aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14')
                        DEVPOS(PROW(),PCOL()+5);DEVOUT('  D ')
                ELSEIF munidade = 'MF'
                        // media final
                        IF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+5);DEVOUT('tran')
                        ELSEIF m_nota[i,26] = 'C'
*                       ELSEIF m_nota[i,1] = '13' .OR. m_nota[i,1] = '14' .OR. m_nota[i,1] = '15'
                                IF m_nota[i,15] >= 6
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT('  AS')
                                ELSE
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT('  AI')
                                ENDIF
                        ELSEIF m_nota[i,15] < 6 .AND. m_nota[i,11] < 6
*                               DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                msituacao := '*'
                                DEVPOS(PROW(),PCOL()+5);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                        ELSE
                                DEVPOS(PROW(),PCOL()+5);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(6),TRANSFORM(m_nota[i,15],'99.9')))
                        ENDIF
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                        LOOP
                ENDIF
                DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
        NEXT
        IF munidade <> 'U1' .AND. munidade <> 'U2' .AND. munidade <> 'U3' .AND. munidade <> 'U4'
                IF mtransf = 'S'
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('TRANSF.')
                ELSEIF msituacao = '*' .AND. munidade = 'MF'
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('REPROV.')
                ELSEIF msituacao = '*'
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('RECUPE.')
                ELSE
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('APROVA.')
                ENDIF
        ENDIF
        SKIP
        WHILE mgstt == aluno->gstt .AND. ! EOF()
                mcont_rec := 0
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
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
*               DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
*               DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
                DEVPOS(PROW()+1,00);DEVOUT(aluno->aluno1)
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                msituacao := ' '
                i := 0
*               DEVPOS(PROW(),42);DEVOUT(' ')
                FOR i = 1 TO LEN(m_nota)
                        IF (m_nota[i,22] = 'D' .AND. m_nota[i,23] = 'D' .AND. m_nota[i,24] = 'D' .AND. m_nota[i,25] = 'D') .OR. ;
                           (aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14')
                                DEVPOS(PROW(),PCOL()+5);DEVOUT('  D ')
                        ELSEIF munidade = 'MF'
                                // media final
                                IF mtransf = 'S'
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT('tran')
                                ELSEIF m_nota[i,26] = 'C'
*                               ELSEIF m_nota[i,1] = '13' .OR. m_nota[i,1] = '14' .OR. m_nota[i,1] = '15'
                                        IF m_nota[i,15] >= 6
                                                DEVPOS(PROW(),PCOL()+5);DEVOUT('  AS')
                                        ELSE
                                                DEVPOS(PROW(),PCOL()+5);DEVOUT('  AI')
                                        ENDIF
                                ELSEIF m_nota[i,15] < 6 .AND. m_nota[i,11] < 6
*                                       DEVPOS(PROW(),PCOL()+1);DEVOUT('*')
                                        msituacao := '*'
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9')))
                                ELSE
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT(IF(EMPTY(m_nota[i,15]),SPACE(6),TRANSFORM(m_nota[i,15],'99.9')))
                                ENDIF
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                                LOOP
                        ENDIF
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                NEXT
                IF munidade <> 'U1' .AND. munidade <> 'U2' .AND. munidade <> 'U3' .AND. munidade <> 'U4'
                        IF mtransf = 'S'
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('TRANSF.')
                        ELSEIF msituacao = '*' .AND. munidade = 'MF'
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('REPROV.')
                        ELSEIF msituacao = '*'
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('RECUPE.')
                        ELSE
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('APROVA.')
                        ENDIF
                ENDIF
                IF PROW() > 56
                        IF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12' .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                                DEVPOS(PROW()+1,00);DEVOUT('Foram cumpridas as exigencias legais quanto aos minimos de assiduidade e carga horaria.')
                        ENDIF
                        imprt('M','N')
                        DEVPOS(PROW()+2,00);DEVOUT('E, para constar eu ..........................., secretario, lavrei a presente ata que vai assinada pelo DIRETOR do estabelecimento.')
                        EJECT
                        imprt('M','N')
                        IF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12' .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (1a. a 4a.) Serie',132))
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (1a. a 4a.) Serie',132))
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Fundamental deste estabelecimento, com os seguintes resultados:')
                        ELSEIF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16' .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18'
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (5a. a 8a.) Serie',132))
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO FUNDAMENTAL (5a. a 8a.) Serie',132))
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Fundamental deste estabelecimento, com os seguintes resultados:')
                        ELSE
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO MEDIO (1o. a 3o.) Ano',132))
                                DEVPOS(PROW(),00);DEVOUT(PADC('ATA DE RESULTADOS FINAIS DO RENDIMENTO ESCOLAR REFERENTE AO ANO LETIVO DE '+ mdiretorio+' ENSINO MEDIO (1o. a 3o.) Ano',132))
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT(' Aos trinta dias do mes de dezembro do ano de '+mdiretorio+' terminou-se o processo de apuracao das notas finais dos alunos da ')
                                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(mgstt,2,1)+' serie '+SUBSTR(mgstt,3,1)+' do turno da manha, do Ensino Medio deste estabelecimento, com os  seguintes resultados:')
                        ENDIF
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT('ATIVIDADES,AREAS DE ESTUDO OU DISCIPLINAS              |')
                        DEVPOS(PROW(),00);DEVOUT('ATIVIDADES,AREAS DE ESTUDO OU DISCIPLINAS              |')
                        i := 0
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(SUBSTR(m_nota[1,2],1,10))
                        FOR i = 2 TO LEN(m_nota)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(SUBSTR(m_nota[i,2],1,10))
                        NEXT
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('RESULT.')
                        i := 0
                        DEVPOS(PROW()+1,0);DEVOUT('Nome do Alunos                                          ')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(SUBSTR(m_nota[1,2],11))
                        FOR i = 2 TO LEN(m_nota)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(SUBSTR(m_nota[i,2],11))
                        NEXT
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('FINAIS')
                        IF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16' .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18' .OR.;
                           SUBSTR(mgstt,1,2) = '21' .OR. SUBSTR(mgstt,1,2) = '22' .OR. SUBSTR(mgstt,1,2) = '23'
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(REPLI('-',132))
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT('TOTAIS DE HORAS DADAS:                                  ')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(ver_ch(mgstt,m_nota[1,1]),'9999'))
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                                i := 0
                                FOR i = 2 TO LEN(m_nota)
                                        DEVPOS(PROW(),PCOL()+5);DEVOUT(TRANSFORM(ver_ch(mgstt,m_nota[i,1]),'9999'))
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' ')
                                NEXT
                        ENDIF
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                        imprt('M','C')
                ENDIF
                SKIP
        ENDDO
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        IF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12' .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                DEVPOS(PROW()+1,00);DEVOUT('Foram cumpridas as exigencias legais quanto aos minimos de assiduidade e carga horaria.')
        ENDIF
        DEVPOS(PROW()+2,00);DEVOUT('E, para constar eu ..........................., secretario, lavrei a presente ata que vai assinada pelo DIRETOR do estabelecimento.')
        EJECT
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        LOOP
ENDDO
**************************** F I M **************************
