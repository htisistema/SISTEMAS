************************************************
* ATA DE ALUNOS P/PROFESSORES
************************************************

FUNCTION saf505
***************
LOCAL mprg:='SAF505',;
      lci,cci,lba,cba,tela,opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mgstt,i,;
      marea,mdisciplina
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO ATA DE ALUNOS P/PROFESSORES','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
lci := 10
cci := 10
lba := 15
cba := 42
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        setcor(3)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Ata p/Profesores ','*')
        setcor(1)
        mgstt := SPACE(4)
        marea := SPACE(1)
        mdisciplina := SPACE(3)
        mquantd := 1
        DEVPOS(lci+1,cci+1);DEVOUT('Digite o GSTT que deseja:')
        DEVPOS(lci+2,cci+1);DEVOUT('Digite a Area...........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Digite a Disciplina.....:')
        DEVPOS(lci+4,cci+1);DEVOUT('Quantidades de Copias...:')
        @ lci+1,cci+27 GET mgstt PICT '@!'
        @ lci+2,cci+27 GET marea PICT '@!'
        @ lci+3,cci+27 GET mdisciplina PICT '@!'
        @ lci+4,cci+27 GET mquantd PICT '99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(6)
        DBSETFILTER({|| aluno->gstt = mgstt})
        GO TOP
        ***************
        IF EOF()
                atencao('Nao existe aluno neste GSTT')
                LOOP
        ENDIF
        IF ! imp_arq('ATA_PROF.REL')
                LOOP
        ENDIF
/*
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N' .OR. LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
        SET PRINT TO 'LPT1'
*/
        i := 0
        FOR i = 1 TO mquantd
                mcont_aluno := 0
                mpag := 1
                mtit := 'Relacao de Alunos'
                mtipo := 'G S T T: '+mgstt
                cabecalho(mpag,mtit,mtipo)
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT('No.')
                DEVPOS(PROW(),06);DEVOUT('Matr.')
                DEVPOS(PROW(),14);DEVOUT('Nome')
                DEVPOS(PROW(),64);DEVOUT('OBSERVACAO')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                GO TOP
                WHILE ! EOF()
                        IF (! EMPTY(marea) .AND. marea <> aluno->area);
                           .OR. (! EMPTY(mdisciplina) .AND. mdisciplina <> aluno->disciplina)
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','N')
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->numero)
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('|')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->area)
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->disciplina)
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('|')
                        IF ! EMPTY(dt_tranf)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('* Transferido *')
                        ENDIF
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                        mcont_aluno ++
                        SKIP
                        IF PROW() >= 58
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT('No.')
                                DEVPOS(PROW(),06);DEVOUT('Matr.')
                                DEVPOS(PROW(),14);DEVOUT('Nome')
                                DEVPOS(PROW(),60);DEVOUT('OBSERVACAO')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                LOOP
                        ENDIF
                ENDDO
                EJECT
                SETPRC(00,00)
        NEXT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('ATA_PROF.REL')
        ENDIF
        CLOSE ALL
        RETURN NIL
ENDDO

