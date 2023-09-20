************************************************
* RELATORIO DE ATUALIZACAO DE ENDERECO
************************************************

FUNCTION safende
***************

LOCAL opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mgstt,mmes,mmatricula,mopcao

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mgstt := SPACE(4)
        mmatricula := SPACE(6)
        mmes := 0
        opcao := mensagem1('Deseja Emitir a Listage [G]->Geral - [S]->GSTT ou [I]ndividual:',' ','G,S,I')
        CLEAR GETS
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        IF opcao = 'G'
                mtit := 'Relacao de Alunos em GERAL'
                mcont_aluno := 0
                mquantd = 1
                mensagem('Escolha ordem que deseja emitir [4]GSTT [3]Alfabetica [4]Matricula:')
                @ 23,COL()+1 GET mquantd PICT '9' VALID IF(mquantd<2 .OR. mquantd>4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(mquantd)
                GO TOP
                ***************
        ELSEIF opcao = 'S'
                mensagem('Digite o GSTT que deseja:')
                @ 23,COL()+1 GET mgstt PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(6)
*                DBSETFILTER({|| aluno->gstt = mgstt})
                GO TOP
                ***************
                IF EOF()
                        atencao('Nao existe aluno neste GSTT')
                        LOOP
                ENDIF
                mtit := 'Relacao de Alunos pelo GSTT: '+mgstt
        ELSE
                mensagem('Digite a Matricula:')
                @ 23,COL()+1 GET mmatricula PICT '@!' VALID ver_aluno(mmatricula,,23,COL()+1)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mtit := 'Atualizacao de endereco do ALUNO'
                ***************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                ***************
                aluno->(DBSEEK(mmatricula))
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        mopcao = 'S'
        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
        READ
        IF mopcao = 'N'
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        mrel := 'LPT1'
        SET DEVI TO PRINT
        SET PRINT TO 'LPT1'
        mcont_aluno := 0
        i := 0
        WHILE ! EOF()
                IF ! EMPTY(dt_tranf)
                         SKIP
                         LOOP
                ENDIF
                IF ! EMPTY(mgstt)
                        IF mgstt <> aluno->gstt
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                INKEY(.3)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Impressao cancelada pelo operador')
                        EXIT
                ENDIF
                imprt('M','N',2)
                imprt('M','E')
                pitalico(.T.)
                DEVPOS(PROW(),00);DEVOUT(memp+resa)
                pitalico(.F.)
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('ATUALIZACAO DE ENDERECO / CADASTRO')
                DEVPOS(PROW(),00);DEVOUT(PADL(mdata_sis,80))
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('*',LEN('ATUALIZACAO DE ENDERECO / CADASTRO')))
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT(mfi+rma)
                DEVPOS(PROW(),00);DEVOUT(PADL(sistema+' '+mversao,136))
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT('*** DADOS DO ALUNO ***')
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Matricula: '+aluno->matricula)
                DEVPOS(PROW(),68);DEVOUT('GSTT: '+aluno->gstt)
                DEVPOS(PROW()+1,00);DEVOUT('Nome: '+aluno->aluno)
                DEVPOS(PROW(),68);DEVOUT('Data Nascimento: '+DTOC(aluno->a_datan))
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                DEVPOS(PROW()+1,00);DEVOUT('*** DADOS DO RESPONSAVEL ***')
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Responsavel: '+aluno->responsa+'  Nasc.: '+DTOC(aluno->r_datnasc))
                DEVPOS(PROW(),PCOL()+1);DEVOUT('CPF: '+aluno->r_cpf)
                DEVPOS(PROW(),PCOL()+1);DEVOUT('Fone: '+aluno->r_fone)
                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+aluno->r_rua+' ,'+aluno->r_num+'-'+r_apto)
                DEVPOS(PROW()+1,00);DEVOUT('Bairro: '+aluno->r_bairro)
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Cidade: '+aluno->r_cidade)
                DEVPOS(PROW(),PCOL()+3);DEVOUT('UF: '+aluno->r_estado)
                DEVPOS(PROW(),PCOL()+3);DEVOUT('CEP: '+r_cep)
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('OBS.: So preencher se alguns dos dados acima estiver errado')
                DEVPOS(PROW()+2,00);DEVOUT('Responsavel: ......................................... Data Nasc.: ...../...../.....')
                DEVPOS(PROW(),PCOL()+1);DEVOUT('CPF: .....................')
                DEVPOS(PROW(),PCOL()+1);DEVOUT('Fone: ...................')
                DEVPOS(PROW()+2,00);DEVOUT('Endereco: ....................................................................  No.:........  Apto.:..........')
                DEVPOS(PROW()+2,00);DEVOUT('Bairro: ................................')
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Cidade: ................................')
                DEVPOS(PROW(),PCOL()+3);DEVOUT('UF: .......')
                DEVPOS(PROW(),PCOL()+3);DEVOUT('CEP: ............... - .......')
                imprt('M','N')
                DEVPOS(PROW()+2,00);DEVOUT(mtraco)
                mcont_aluno ++
                IF mcont_aluno = 2
                        EJECT
                        mcont_aluno := 0
                        SKIP
                        LOOP
                ENDIF
                imprt('M','N',4)
                DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                DEVPOS(PROW(),00);DEVOUT(PADC('Corte aqui',80))
                DEVPOS(PROW()+3,00);DEVOUT(' ')
                SKIP
                IF opcao = 'I'
                        EXIT
                ENDIF
        ENDDO
        imprt('M','N',1)
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        RETURN NIL
ENDDO

