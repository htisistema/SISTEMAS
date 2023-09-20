***********************************
* FUNCAO PARA ATUALIZAR OS DBF'S
***********************************

FUNCTION saf_dbf
****************

LOCAL matmov:={ },matcad:={ },matpeds:={ },matpede:={ },matdupr:={ },matcli:={ },;
      matsen:={ },matcai:={},matfor:={}

LOCAL mmerc,mmov,mpeds,mpede,mdupr,mcli,msetup,mcaixa,mcartao,mcor,mforn,mdupp

PRIVATE mcliente

REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
mold := 'ÚÄ¿³ÙÄÀ³'      // Molduras das janelas

SET MESSAGE TO 24 CENTER
SET CONS OFF
SET WRAP ON
SET TALK OFF
SET SCOREBOARD OFF
SET DATE BRITI
SET DELE ON
SET INTEN ON
mcor := SETCOLOR()

WHILE .T.
        mmerc := SPACE(1)
        mmov := SPACE(1)
        mpeds := SPACE(1)
        mpede := SPACE(1)
        mdupr := SPACE(1)
        mdupp := SPACE(1)
        mcli := SPACE(1)
        msetup:= SPACE(1)
        mcaixa:= SPACE(1)
        mcartao:= SPACE(1)
        mforn := SPACE(1)
        mcor := setcor(1)

        CLS
        SETCOR(6)
        @ 01,01 SAY 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
        @ 02,01 SAY '³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³Û'
        @ 03,01 SAY '³    ÄÄÄÄÜÜÄÄÄÄ    ³Û'
        @ 04,01 SAY '³     ÄÄÄÜÜÄÄÄ     ³Û'
        @ 05,01 SAY '³    ÄÄÄÄÜÜÄÄÄÄ    ³Û'
        @ 06,01 SAY '³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³Û'
        @ 07,01 SAY '³                  ³Û'
        @ 08,01 SAY 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ'
        @ 09,01 SAY ' ßßßßßßßßßßßßßßßßßßßß'
        SETCOR(3)
        @ 01,31 SAY 'ÜÜÜ ÜÜÜ Ü ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜÜ '
        @ 02,31 SAY ' Û  Û Û Û ÛÜÜÜ ÛÜ   ÛÜÜÛ Û    Û  Û Û Û Û '
        @ 03,31 SAY 'ÜÛÜ Û ÛÜÛ ÜÜÜÛ ÛÜÜÜ Û ÛÜ ÛÜÜÜ ÛÜÜÛ Û Û Û '
        @ 04,31 SAY 'ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ'
        @ 06,31 SAY '          A T U A L I Z A C A O          '
        @ 09,00 SAY REPLI('ß',80)
        SETCOR(1)
        DEVPOS(10,00);DEVOUT('1> Atualizar o arquivo CADALUNO.DBF    [ ]  24.09.01')
        DEVPOS(11,00);DEVOUT('2> Atualizar o arquivo CDMENSAL.DBF    [ ]  01.02.06')
        DEVPOS(12,00);DEVOUT('3> Atualizar o arquivo SAFCAIXA.DBF    [ ]  18.05.2003')
        DEVPOS(13,00);DEVOUT('4> Atualizar o arquivo FAIXA.DBF       [ ]  16/03/98')
        DEVPOS(14,00);DEVOUT('5> Atualizar o arquivo SAFSETUP.DBF    [ ]  03.08.2006')
        DEVPOS(24,00);DEVOUT('Marque com um "X" os Arquivos que deseja atualizar')

        @ 10,40 GET mmerc PICT '@!' VALID mmerc $ 'X, '
        @ 11,40 GET mmov PICT '@!' VALID mmov $ 'X, '
        @ 12,40 GET mpeds PICT '@!' VALID mpeds $ 'X, '
        @ 13,40 GET mpede PICT '@!' VALID mpede $ 'X, '
        @ 14,40 GET mdupr PICT '@!' VALID mdupr $ 'X, '
/*
        @ 18,40 GET mdupp PICT '@!' VALID mdupp $ 'X, '
        @ 19,40 GET mcli PICT '@!' VALID mcli $ 'X, '
        @ 20,40 GET msetup PICT '@!' VALID msetup $ 'X, '
        @ 21,40 GET mcaixa PICT '@!' VALID mcaixa $ 'X, '
        @ 22,40 GET mcartao PICT '@!' VALID mcartao $ 'X, '
        @ 23,40 GET mforn PICT '@!' VALID mforn $ 'X, '
*/
        READ
        IF LASTKEY() = 27
                mensagem('Deseja Finalizar a opera‡Æo [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'S'
                        SETCOLOR('w/n,,N,x')
                        CLEAR
                        QUIT
                ELSE
                        LOOP
                ENDIF
        ENDIF
        mensagem('Confirma a Atualiza‡Æo [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                mensagem('Deseja Finalizar a opera‡Æo [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'S'
                        SETCOLOR('w/n,,N,x')
                        CLEAR
                        QUIT
                ELSE
                        LOOP
                ENDIF
        ENDIF
        IF opcao = 'N'
                LOOP
        ENDIF
        limpa(13,00,24,79)
        SETPOS(12,00)
        IF mmerc = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo CADALUNO.DBF.. Aguarde um momento OK')
                rename CADALUNO.DBF TO CADALUNV.DBF
                matcad:={ }
                AADD(matcad,{'contrato','C',8,0})
                AADD(matcad,{'numero','C',3,0})
                AADD(matcad,{'gstt','C',4,0})
                AADD(matcad,{'aluno','C',40,0})
                AADD(matcad,{'aluno1','C',55,0})
                AADD(matcad,{'matricula','C',6,0})
                AADD(matcad,{'sexo','C',1,0})
                AADD(matcad,{'a_datan','D',8,0})
                AADD(matcad,{'a_natur','C',14,0})
                AADD(matcad,{'a_estado','C',2,0})
                AADD(matcad,{'a_nacion','C',8,0})
                AADD(matcad,{'data_ing','D',8,0})
                AADD(matcad,{'ing_esco','D',8,0})
                AADD(matcad,{'pai','C',40,0})
                AADD(matcad,{'p_fone','C',8,0})
                AADD(matcad,{'mae','C',40,0})
                AADD(matcad,{'m_fone','C',8,0})
                AADD(matcad,{'responsa','C',40,0})
                AADD(matcad,{'r_rua','C',35,0})
                AADD(matcad,{'r_num','C',5,0})
                AADD(matcad,{'r_apto','C',5,0})
                AADD(matcad,{'r_bairro','C',20,0})
                AADD(matcad,{'r_cidade','C',20,0})
                AADD(matcad,{'r_estado','C',2,0})
                AADD(matcad,{'r_cep','C',8,0})
                AADD(matcad,{'r_fone','C',8,0})
                AADD(matcad,{'r_prof','C',20,0})
                AADD(matcad,{'r_cpf','C',12,0})
                AADD(matcad,{'r_fone1','C',8,0})
                AADD(matcad,{'r_fone2','C',8,0})
                AADD(matcad,{'r_ramal','C',4,0})
                AADD(matcad,{'r_datnasc','D',8,0})
                AADD(matcad,{'area','C',1,0})
                AADD(matcad,{'disciplina','C',3,0})
                AADD(matcad,{'bloqueio','C',1,0})
                AADD(matcad,{'bloqueio2','C',1,0})
                AADD(matcad,{'bloqueio3','C',1,0})
                AADD(matcad,{'bloqueio4','C',1,0})
                AADD(matcad,{'dt_trans', 'D',8,0})
                AADD(matcad,{'faixa_1','C',1,0})
                AADD(matcad,{'faixa_2','C',1,0})
                AADD(matcad,{'faixa_3','C',1,0})
                AADD(matcad,{'faixa_4','C',1,0})
                AADD(matcad,{'faixa_5','C',1,0})
                AADD(matcad,{'faixa_6','C',1,0})
                AADD(matcad,{'faixa_7','C',1,0})
                AADD(matcad,{'faixa_8','C',1,0})
                AADD(matcad,{'faixa_9','C',1,0})
                AADD(matcad,{'faixa_10','C',1,0})
                AADD(matcad,{'faixa_11','C',1,0})
                AADD(matcad,{'faixa_12','C',1,0})
                AADD(matcad,{'dt_m_fai','D',8,0})
                AADD(matcad,{'dt_tranf','D',8,0})
                AADD(matcad,{'op_tranf','C',3,0})
                AADD(matcad,{'dt_exclu','D',8,0})
                AADD(matcad,{'op_exclu','C',3,0})
                AADD(matcad,{'flag','C',1,0})
                AADD(matcad,{'obs','C',50,0})
                DBCREATE('cadaluno.dbf',matcad)
                ERASE cadaluno.cdx
                USE cadaluno ALIAS aluno NEW
                **************
                SELE('aluno')
                **************
                BLOQARQ()
                APPEND FROM CADALUNV.DBF
                DBCOMMIT()
                DBUNLOCK()
                CLOSE ALL
                ERASE CADALUNV.dbf
        ENDIF
        IF mmov = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo CDMENSAL.DBF.. Aguarde um momento OK')
                rename CDMENSAL.DBF TO CDMENSAV.DBF
                matriz:={ }
                AADD(matriz,{'ano','C',4,0})
                AADD(matriz,{'contrato','C',8,0})
                AADD(matriz,{'tipo','C',2,0})
                AADD(matriz,{'gstt','C',4,0})
                AADD(matriz,{'matricula','C',6,0})
                AADD(matriz,{'aluno','C',40,0})
                AADD(matriz,{'bolsa','C',1,0})
                AADD(matriz,{'parcela','C',2,0})
                AADD(matriz,{'data_pro','D',8,0})
                AADD(matriz,{'data_venc','D',8,0})
                AADD(matriz,{'data_pag','D',8,0})
                AADD(matriz,{'vlr_parc','N',10,2})
                AADD(matriz,{'valor','N',10,2})
                AADD(matriz,{'multa','N',10,2})
                AADD(matriz,{'compl','N',10,2})
                AADD(matriz,{'desc_bolsa','N',10,2})
                AADD(matriz,{'flag','C',1,0})
                AADD(matriz,{'operador','C',3,0})
                AADD(matriz,{'conta','C',1,0})
                AADD(matriz,{'nosso_no','C',7,0})
                AADD(matriz,{'nosso_dv','C',1,0})
                DBCREATE('cdmensal.dbf',matriz)
                ERASE cdmensal.cdx
                USE cdmensal ALIAS men NEW EXCLUSIVE
                **************
                SELE('men')
                **************
                BLOQARQ()
                APPEND FROM CDMENSAV.DBF
                COMMIT
                UNLOCK
                CLOSE ALL
                ERASE cdmensal.cdx
                ERASE cdmensav.dbf
        ENDIF
        IF mpeds = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo SAFCAIXA.DBF.. Aguarde um momento OK')
                rename SAFCAIXA.DBF TO SAFCAIXV.DBF
                matriz:={ }
                AADD(matriz,{'ano','C',4,0})
                AADD(matriz,{'tipo','C',2,0})
                AADD(matriz,{'data','D',8,0})
                AADD(matriz,{'nota','C',8,0})
                AADD(matriz,{'num_dup','C',10,0})
                AADD(matriz,{'num_ban','C',3,0})
                AADD(matriz,{'cod_ct','C',3,0})
                AADD(matriz,{'documento','C',10,0})
                AADD(matriz,{'venci','D',8,0})
                AADD(matriz,{'valor','N',10,2})
                AADD(matriz,{'vlr_multa','N',10,2})
                AADD(matriz,{'vlr_juros','N',10,2})
                AADD(matriz,{'vlr_igpm','N',10,2})
                AADD(matriz,{'tipo_comp','C',2,0})
                AADD(matriz,{'coddesp','C',3,0})
                AADD(matriz,{'descri1','C',40,0})
                AADD(matriz,{'descri2','C',40,0})
                AADD(matriz,{'cod_vend','C',3,0})
                AADD(matriz,{'cod_cli','C',6,0})
                AADD(matriz,{'cod_opera','C',3,0})
                AADD(matriz,{'sangria','C',1,0})
                AADD(matriz,{'op_pg','C',3,0})
                AADD(matriz,{'pg','C',1,0})
                AADD(matriz,{'hora','C',8,0})
                AADD(matriz,{'autoriz','C',3,0})
                AADD(matriz,{'valor_com','N',10,2})
                AADD(matriz,{'comissao','N',12,6})
                AADD(matriz,{'tp_desp','C',1,0})
                AADD(matriz,{'num_pdv','C',4,0})
                AADD(matriz,{'num_cup','C',6,0})
                DBCREATE('safcaixa.dbf',matriz)
                USE safcaixa ALIAS caix NEW EXCLUSIVE
                **************
                SELE('caix')
                **************
                BLOQARQ()
                APPEND FROM SAFCAIXV.DBF
                COMMIT
                UNLOCK
                ERASE safcaixa.cdx
                ERASE safcaixv.dbf
                DBUNLOCKALL()
                DBCOMMITALL()
                CLOSE ALL
        ENDIF
        IF mpede = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo FAIXA.DBF.. Aguarde um momento OK')
                rename FAIXA.DBF TO FAIXAV.DBF
                matriz:={ }
                AADD(matriz,{'faixa','C',1,0})
                AADD(matriz,{'desconto','N',3,0})
                AADD(matriz,{'descricao','C',10,0})
                AADD(matriz,{'operador','C',3,0})
                DBCREATE('faixa.dbf',matriz)
                USE faixa ALIAS fx NEW EXCLUSIVE
                **************
                SELE('fx')
                **************
                BLOQARQ()
                APPEND FROM FAIXAV.DBF
                DBCOMMITALL()
                DBUNLOCKALL()
                CLOSE ALL
                ERASE faixa.cdx
                ERASE faixav.dbf
        ENDIF
        IF mdupr = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo SAFSETUP.. Aguarde um momento OK')
                rename SAFSETUP.DBF TO SAFSETUV.DBF
                matriz:={ }
                AADD(matriz,{'porta','C',4,0})
                AADD(matriz,{'vlr_multa','N',15,5})
                AADD(matriz,{'vlr_juros','N',15,5})
                AADD(matriz,{'sysdatlm','C',12,0})
                DBCREATE('safsetup.dbf',matriz)
                USE safsetup ALIAS set NEW EXCLUSIVE
                **************
                SELE('set')
                **************
                BLOQARQ()
                APPEND FROM safsetuv.DBF
                DBUNLOCKALL()
                DBCOMMITALL()
                ERASE safsetuv.dbf
                CLOSE ALL
        ENDIF
        IF mdupp = 'X'
                CLOSE ALL
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo SACDUPP.. Aguarde um momento OK')
                rename SACDUPP.DBF TO SACDUPPV.DBF
                matdupp:={ }
                AADD(matdupp,{'tipo','C',2,0})
                AADD(matdupp,{'numero','C',3,0})
                AADD(matdupp,{'duplicata','C',10,0})
                AADD(matdupp,{'valor_dup','N',10,2})
                AADD(matdupp,{'fornec','C',4,0})
                AADD(matdupp,{'cliente','C',40,0})
                AADD(matdupp,{'emissao','D',8,0})
                AADD(matdupp,{'venc','D',8,0})
                AADD(matdupp,{'datpag','D',8,0})
                AADD(matdupp,{'banco','C',3,0})
                AADD(matdupp,{'valor','N',10,2})
                AADD(matdupp,{'vlpago','N',10,2})
                AADD(matdupp,{'desc','N',10,2})
                AADD(matdupp,{'juros','N',10,2})
                AADD(matdupp,{'pago','C',1,0})
                AADD(matdupp,{'conta','C',1,0})
                AADD(matdupp,{'obs','C',60,0})
                AADD(matdupp,{'nbanco','C',4,0})
                AADD(matdupp,{'cheque','C',8,0})
                AADD(matdupp,{'vendedor','C',3,0})
                AADD(matdupp,{'num_ped','C',6,0})
                DBCREATE('sacdupp.dbf',matdupp)
                ERASE sacdupp.cdx
                USE sacdupp ALIAS dupp NEW EXCLUSIVE
                **************
                SELE('dupp')
                **************
                BLOQARQ()
                APPEND FROM SACDUPPV.DBF
                DBUNLOCKALL()
                GO TOP
                WHILE ! EOF()
                        dupp->valor_dup := dupp->valor
                        SKIP
                ENDDO
                DBUNLOCKALL()
                DBCOMMITALL()
                CLOSE dupp
                ERASE sacdupp.cdx
                ERASE sacforn.cdx
                DBCOMMITALL()
                DBUNLOCKALL()
                CLOSE ALL
                ERASE sacduppv.dbf
        ENDIF
        IF mcli = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo SACCLI.DBF.. Aguarde um momento OK')
        ENDIF
        IF mforn = 'X'
                DEVPOS(ROW()+1,00);DEVOUT('** Atualizando o arquivo INSOPERA.DBF.. Aguarde um momento OK')
                rename INSOPERA.DBF TO INSOPERV.DBF
                matsen:={ }
                AADD(matsen,{'scod_op','C',3,0})
                AADD(matsen,{'snome','C',30,0})
                AADD(matsen,{'ssenha','C',8,0})
                AADD(matsen,{'sdata_cad','D',8,0})
                AADD(matsen,{'scomissao','N',6,2})
                AADD(matsen,{'scom_praz','N',6,2})
                AADD(matsen,{'scota','N',12,2})
                AADD(matsen,{'sult_ent','D',8,0})
                AADD(matsen,{'snivel','C',1,0})
                AADD(matsen,{'shora_ini','C',5,2})
                AADD(matsen,{'shora_fim','C',5,2})
                AADD(matsen,{'sult_e_c','D',8,0})
                AADD(matsen,{'sh_i_c','C',5,2})
                DBCREATE('insopera.dbf',matsen)
                ERASE insopera.cdx
                USE insopera ALIAS sen NEW EXCLUSIVE
                **************
                SELE('sen')
                **************
                BLOQARQ()
                APPEND FROM insoperv.DBF
                DBCOMMITALL()
                DBUNLOCKALL()
                CLOSE ALL
                ERASE insoperv.dbf
        ENDIF
        SETPOS(ROW()+1,00)
        RUN ERASE *.ntx
        RUN ERASE TEXTO.dbf
        RUN ERASE TEXTO.dbt
        atencao('Atualiza‡Æo Completa com exito !!!')
        SETCOLOR('w/n,,N,x')
        CLEAR
        quit
ENDDO
RETURN NIL
