************************************************
* RELATORIO DE ALUNOS BOLSISTA ENDERECO
************************************************

FUNCTION saf5233
***************
LOCAL mprg:='SAF5233'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mtipo_fx,mordem,mtipo_vlr,mgstt:=SPACE(4)

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS BOLSISTAS E ENDERECO','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',130)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mcont_aluno := 0
        mtot_men := 0
        mtot_des := 0
        mger_men := 0
        mger_des := 0
        mger_aluno := 0
        mmes := SPACE(2)
        mquantd = 1
        mfaixa := SPACE(2)
        mtipo_fx := SPACE(1)
        mordem := 1
        mtipo_vlr := 'S'
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
        mensagem('Informa a Bolsa que deseja ou ENTER p/todos [A...Z]:')
        @ 23,COL()+1 GET mtipo_fx    //VALID IF(EMPTY(mtipo_fx),.F.,.T.)
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        mensagem('Digite o mes que Deseja [01...12]:')
        opcao = 'S'
        @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Tipo de ordem [1]-Respons. [2]-GSTT [3]-Aluno [4]-Matricula:')
        opcao = 'S'
        @ 23,COL()+1 GET mordem PICT '9' RANGE 1,4
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Deseja imprimir o valor da BOLSA:')
        mtipo_vlr = 'S'
        @ 23,COL()+1 GET mtipo_vlr PICT '@!' VALID mtipo_vlr $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Digite o GSTT ou deixe em branco p/todos:')
        mgstt = '    '
        @ 23,COL()+1 GET mgstt PICT '@!' VALID ver_gstt(mgstt,,'*')
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(mordem)
        GO TOP
        ***************
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('BOLSA_IN.REL')
                LOOP
        ENDIF
        mfaixa := ver_faixa(mmes)
        mpag := 0
        WHILE ! EOF()
                IF ! EMPTY(mtipo_fx)
                        IF ver_faixa(mmes) <> mtipo_fx
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                mdesconto := faixa_vlr(mtipo_fx)
                IF PROW() > 57 .OR. mpag = 0
                        IF mpag > 0
                                EJECT
                        ENDIF
                        mpag := mpag + 1
                        mtit := 'Relacao dos Alunos Bolsista do mes: '+mmes
                        mtipo := 'Valor da Bolsa: '+TRANSFORM(mdesconto,'999.99')+' %'
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT(' Matric.')
                        DEVPOS(PROW(),10);DEVOUT('Aluno')
                        IF mtipo_vlr = 'S'
                                DEVPOS(PROW(),49);DEVOUT('Valor Bolsa')
                        ENDIF
                        DEVPOS(PROW(),62);DEVOUT('Responsavel')
                        DEVPOS(PROW(),104);DEVOUT('C.P.F.')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                IF ! EMPTY(mgstt) .AND. mgstt <> aluno->gstt
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(aluno->dt_tranf)
                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),mmes)
                mdesconto := faixa_vlr(ver_faixa(mmes))
                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                IF mtipo_vlr = 'S'
                        DEVPOS(PROW(),52);DEVOUT(TRANSFORM(mvlr_men - (mvlr_men * (mdesconto/100)),'9,999.99'))
                ENDIF
                DEVPOS(PROW(),62);DEVOUT(aluno->responsa)
                DEVPOS(PROW(),104);DEVOUT(aluno->r_cpf)
                DEVPOS(PROW()+1,00);DEVOUT('Filiacao do Aluno: PAI: '+RTRIM(pai)+' - MAE: '+RTRIM(mae))
                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+RTRIM(aluno->r_rua)+', '+RTRIM(r_num)+'- Compl.:'+r_apto+' - '+RTRIM(r_bairro)+' - '+RTRIM(r_cidade)+' - '+r_estado+' - '+r_cep+' - Fone:'+r_fone)
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',130))
                mcont_aluno ++
                mtot_men := mtot_men + mvlr_men
                mtot_des := mtot_des + (mvlr_men * (mdesconto/100))
                mger_aluno ++
                mger_men := mger_men + mvlr_men
                mger_des := mger_des + (mvlr_men * (mdesconto/100))
                SKIP
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+2,00);DEVOUT('T O T A L    G E R A L')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mger_aluno,'9,999.99'))
        IF mtipo_vlr = 'S'
               DEVPOS(PROW()+1,00);DEVOUT('Total a Receber.....: '+TRANSFORM(mger_men-mger_des,'999,999.99'))
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('BOLSA_IN.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL
**************************** F I M **************************

