************************************************
* RELATORIO DE ALUNOS P/SEGURO
************************************************

FUNCTION saf5291
***************
LOCAL mprg:='SAF5291'
LOCAL opcao,mtraco,mtit,mtipo,mcont_aluno,mend,morde
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)
IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS P/SEGURADORA (1)','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mcont_aluno := 0
        mend := SPACE(1)
        morde := 1
        mensagem('Com Endereco [S/N]:')
        @ 23,COL()+1 GET mend PICT '@!'
        READ
        mensagem('Ordem do Relatorio [1]Responsavel [3]Alfabetica de Aluno:')
        @ 23,COL()+1 GET morde PICT '9'
        READ
        IF LASTKEY() = 27
                CLOSE ALL
                RETURN NIL
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(morde)
        GO TOP
        ***************
        IF ! imp_arq('ALU_SEG.REL')
                LOOP
        ENDIF
*        SET DEVI TO PRINT
        * * SET PRINT TO LPT1
        mpag := 1
        mtit := 'Relacao de Alunos para Seguradora (1)'
        IF morde = 1
                mtipo := 'por ordem de Responsavel'
        ELSE
                mtipo := 'por ordem de Alfabetica de Aluno'
        ENDIF
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
        DEVPOS(PROW(),42);DEVOUT('CPF')
        DEVPOS(PROW(),56);DEVOUT('Fone')
        DEVPOS(PROW(),66);DEVOUT('Aluno')
        DEVPOS(PROW(),108);DEVOUT('Data Nasc.')
        DEVPOS(PROW(),120);DEVOUT('GSTT')
        DEVPOS(PROW(),126);DEVOUT('Faixa')
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                IF ! EMPTY(dt_tranf) .OR. aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                DEVPOS(PROW(),42);DEVOUT(aluno->r_cpf)
                DEVPOS(PROW(),56);DEVOUT(aluno->r_fone)
                DEVPOS(PROW(),66);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),108);DEVOUT(aluno->a_datan)
                DEVPOS(PROW(),120);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),128);DEVOUT(ver_faixa(SUBSTR(DTOC(mdata_sis),4,2)))
*               DEVPOS(PROW(),128);DEVOUT(aluno->faixa_1)
                IF mend = 'S'
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+','+aluno->r_num+'-';
                        +aluno->r_apto+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+;
                        aluno->r_estado+' - CEP: '+aluno->r_cep)
                ENDIF
                mcont_aluno ++
                SKIP
                IF PROW() >= 59
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                        DEVPOS(PROW(),42);DEVOUT('CPF')
                        DEVPOS(PROW(),56);DEVOUT('Fone')
                        DEVPOS(PROW(),66);DEVOUT('Aluno')
                        DEVPOS(PROW(),108);DEVOUT('Data Nasc.')
                        DEVPOS(PROW(),120);DEVOUT('GSTT')
                        DEVPOS(PROW(),126);DEVOUT('Faixa')
                        imprt('M','N')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+STRZERO(mcont_aluno,5))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('ALU_SEG.REL')
        ENDIF
        CLOSE ALL
        RETURN NIL
ENDDO

