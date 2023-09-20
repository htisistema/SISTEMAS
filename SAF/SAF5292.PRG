************************************************
* RELATORIO DE ALUNOS P/SEGURO (2)
************************************************

FUNCTION saf5292
***************
LOCAL mprg:='SAF5292'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mend
PRIVATE mtipo_imp:='M'
IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS P/SEGURADORA (2)','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mcont_aluno := 0
        mquantd = 1
        mend := SPACE(1)
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        mensagem('Com Endereco [S/N]:')
        @ 23,COL()+1 GET mend PICT '@!'
        READ
        mensagem('Quantidade de Copias:')
        @ 23,COL()+1 GET mquantd PICT '999'
        READ
        IF LASTKEY() = 27
                CLOSE ALL
                RETURN NIL
        ENDIF
/*
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N'
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
*/
        IF ! imp_arq('ALU_SEG.REL')
                LOOP
        ENDIF
*        SET DEVI TO PRINT
*        SET PRINT TO 'teste.rel'
        mpag := 1
        mtit := 'Relacao de Alunos para Seguradora (2)'
        mtipo := 'por ordem de Responsavel'
        cabecalho(mpag,mtit,mtipo)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                IF ! EMPTY(dt_tranf) .OR. aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Aluno: '+aluno->matricula+'-'+aluno->aluno)
                DEVPOS(PROW(),PCOL()+2);DEVOUT('Responsavel: '+aluno->responsa)
                DEVPOS(PROW(),PCOL()+2);DEVOUT('GSTT: '+aluno->gstt)
                DEVPOS(PROW()+1,00);DEVOUT('PAI..: '+aluno->pai)
                DEVPOS(PROW(),PCOL()+9);DEVOUT('MAE........: '+aluno->mae)
                DEVPOS(PROW(),PCOL()+2);DEVOUT('FONE: '+aluno->r_fone)
                IF mend = 'S'
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+','+aluno->r_num+'-';
                        +aluno->r_apto+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+;
                        aluno->r_estado+' - CEP: '+aluno->r_cep)
                ENDIF
                mcont_aluno ++
                SKIP
                IF PROW() >= 58
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('Quantidade de Alunos: '+STRZERO(mcont_aluno,5))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        RETURN NIL
ENDDO

