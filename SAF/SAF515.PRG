************************************************
* EMISSAO DO MAPA NOTAS POR TURMA/UNIDADE
************************************************

FUNCTION saf515
***************
LOCAL mprg:='SAF515'
LOCAL lci,cci,lba,cba,opcao,tela,i,mgstt,mtipo_con,mtransf,mtraco,msit_nota,mvlr_nota:=0
PRIVATE m_nota := {},munidade,mcont_rec,mtipo_sit:=SPACE(20),m_aluno:={},mqtd:=0
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DOS 10 MELHORES NOTAS P/GSTT','125')
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
        msit_nota := ' '
        munidade := SPACE(2)
        mtipo_con := 1
        mgstt := SPACE(4)
        mcont_rec := 0
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' Os 10 Melhores ','*')
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Opcao: [U1] [U2] [U3] [U4] [MU]')
        setcor(1)
        *****************
        SELE('aluno');ORDSETFOCUS(6)
        GO TOP
        *****************
        DEVPOS(lci+1,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+1,cci+14);DEVOUT('Unidade:')
        DEVPOS(lci+3,cci+1);DEVOUT('Quantidade de Alunos p/ser Impresso:')
        @ lci+1,cci+7 GET mgstt PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! aluno->(DBSEEK(mgstt))
                atencao('Nao foi possivel encontrar esta TURMA')
                LOOP
        ENDIF
        @ lci+1,cci+23 GET munidade PICT '@!' WHEN men_get(lci+2,cci+23,'Informe o Tipo de Unidade que deseja [U1]1a.unidade [U2]2a.unidade [U3]3a.unidade [U4]4a.unidade [MU]Media das Unidades') VALID lim_get() .AND. munidade $ 'U1,U2,U3,U4,MU'
        @ lci+3,cci+38 GET mqtd PICT '999'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('MED_10.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'OS 10 MELHORES'
        mtipo := 'GRAU: '+SUBSTR(mgstt,1,1)+' SERIE: '+SUBSTR(mgstt,2,1)+' TURMA: '+SUBSTR(mgstt,3,1)+' TURNO: '+SUBSTR(mgstt,4,1)+'  UNIDADE: '+munidade
        cabecalho(mpag,mtit,mtipo,,132)
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT('Matricula')
        DEVPOS(PROW(),12);DEVOUT('Nome do Aluno')
        DEVPOS(PROW(),60);DEVOUT('Media das Notas')
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
        ASIZE(m_nota,0)
        mvlr_nota :=0
        ASIZE(m_aluno,0)
        mtransf := SPACE(1)
        IF ! EMPTY(aluno->dt_tranf)
                mtransf := 'S'
        ENDIF
        boletim(aluno->matricula,munidade)
        WHILE LEN(m_nota) = 0
                SKIP
                boletim(aluno->matricula,munidade)
        ENDDO
        msit_nota := ' '
        i := 0
        AADD(m_aluno,{mvlr_nota/LEN(m_nota),aluno->matricula,aluno->aluno})
        mtransf := SPACE(1)
        IF ! EMPTY(aluno->dt_tranf)
                mtransf := 'S'
        ENDIF
        i := 0
        FOR i = 1 TO LEN(m_nota)
                IF munidade = 'U1'
                        IF EMPTY(m_nota[i,3]) .AND. ! EMPTY(m_nota[i,22])
                                LOOP
                        ELSEIF mtransf = 'S'
                                LOOP
                        ELSE
                                mvlr_nota := mvlr_nota + m_nota[i,3]
                        ENDIF
                ELSEIF munidade = 'U2'
                        IF EMPTY(m_nota[i,5]) .AND. ! EMPTY(m_nota[i,23])
                                LOOP
                        ELSEIF mtransf = 'S'
                                LOOP
                        ELSE
                                mvlr_nota := mvlr_nota + m_nota[i,5]
                        ENDIF
                ELSEIF munidade = 'U3'
                        IF EMPTY(m_nota[i,7]) .AND. ! EMPTY(m_nota[i,24])
                                LOOP
                        ELSEIF mtransf = 'S'
                                LOOP
                        ELSE
                                mvlr_nota := mvlr_nota + m_nota[i,7]
                        ENDIF
                ELSEIF munidade = 'U4'
                        IF EMPTY(m_nota[i,9]) .AND. ! EMPTY(m_nota[i,25])
                                LOOP
                        ELSEIF mtransf = 'S'
                                LOOP
                        ELSE
                                mvlr_nota := mvlr_nota + m_nota[i,7]
                        ENDIF
                ELSEIF munidade = 'MU'          //.OR. m_nota[i,11] >= 6
                        IF mtransf = 'S'
                                LOOP
                        ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                LOOP
                        ELSE
                                mvlr_nota := mvlr_nota + m_nota[i,11]
                        ENDIF
                ENDIF
        NEXT
        AADD(m_aluno,{mvlr_nota/LEN(m_nota),aluno->matricula,aluno->aluno})
        SKIP
        WHILE mgstt == aluno->gstt .AND. ! EOF()
                mcont_rec := 0
                ASIZE(m_nota,0)
                mvlr_nota :=0
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                boletim(aluno->matricula,munidade)
                WHILE LEN(m_nota) = 0
                        SKIP
                        boletim(aluno->matricula,munidade)
                ENDDO
                msit_nota := ' '
                mtransf := SPACE(1)
                IF ! EMPTY(aluno->dt_tranf)
                        mtransf := 'S'
                ENDIF
                i := 0
                FOR i = 1 TO LEN(m_nota)
                        IF munidade = 'U1'
                                IF EMPTY(m_nota[i,3]) .AND. ! EMPTY(m_nota[i,22])
                                        LOOP
                                ELSEIF mtransf = 'S'
                                        LOOP
                                ELSE
                                        mvlr_nota := mvlr_nota + m_nota[i,3]
                                ENDIF
                        ELSEIF munidade = 'U2'
                                IF EMPTY(m_nota[i,5]) .AND. ! EMPTY(m_nota[i,23])
                                        LOOP
                                ELSEIF mtransf = 'S'
                                        LOOP
                                ELSE
                                        mvlr_nota := mvlr_nota + m_nota[i,5]
                                ENDIF
                        ELSEIF munidade = 'U3'
                                IF EMPTY(m_nota[i,7]) .AND. ! EMPTY(m_nota[i,24])
                                        LOOP
                                ELSEIF mtransf = 'S'
                                        LOOP
                                ELSE
                                        mvlr_nota := mvlr_nota + m_nota[i,7]
                                ENDIF
                        ELSEIF munidade = 'U4'
                                IF EMPTY(m_nota[i,9]) .AND. ! EMPTY(m_nota[i,25])
                                        LOOP
                                ELSEIF mtransf = 'S'
                                        LOOP
                                ELSE
                                        mvlr_nota := mvlr_nota + m_nota[i,7]
                                ENDIF
                        ELSEIF munidade = 'MU'          //.OR. m_nota[i,11] >= 6
                                IF mtransf = 'S'
                                        LOOP
                                ELSEIF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        LOOP
                                ELSE
                                        mvlr_nota := mvlr_nota + m_nota[i,11]
                                ENDIF
                        ENDIF
                NEXT
                AADD(m_aluno,{mvlr_nota/LEN(m_nota),aluno->matricula,aluno->aluno})
                SKIP
        ENDDO
        ASORT(m_aluno,,,{|x,y| x[1] > y[1]})
        i := 0
        IF EMPTY(mqtd) .OR. mqtd > LEN(m_aluno)
                mqtd := LEN(m_aluno)
        ENDIF
        FOR i = 1 TO mqtd
                DEVPOS(PROW()+1,00);DEVOUT(m_aluno[i,2])
                DEVPOS(PROW(),12);DEVOUT(m_aluno[i,3])
                DEVPOS(PROW(),64);DEVOUT(TRANSFORM(m_aluno[i,1],'999.99'))
        NEXT
        EJECT
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('MED_10.REL')
        ENDIF
        LOOP
ENDDO
**************************** F I M **************************
