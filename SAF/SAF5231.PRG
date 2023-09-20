************************************************
* RELATORIO DE ALUNOS BOLSISTA
************************************************

FUNCTION saf5231
***************
LOCAL mprg:='SAF5231'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mtipo_fx,mordem,mgstt:=SPACE(4),lci:=10,cci:=10,lba:=16,cba:=70

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS BOLSISTAS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
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
        mgstt := SPACE(4)
        opcao = 'S'
        mgstt = '    '
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
        botao(lci,cci,lba,cba)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite Bolsa ou ENTER p/todos.............:')
        DEVPOS(lci+2,cci+1);DEVOUT('Digite o mes que Deseja...................:')
        DEVPOS(lci+3,cci+1);DEVOUT('Ordem [1]Resp. [2]GSTT [3]Aluno [4]Matric.:')
        DEVPOS(lci+4,cci+1);DEVOUT('Digite o GSTT ou deixe em branco p/todos..:')
        @ lci+1,cci+45 GET mtipo_fx     //VALID IF(EMPTY(mtipo_fx),.F.,.T.)
        @ lci+2,cci+45 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+3,cci+45 GET mordem PICT '9' RANGE 1,4
        @ lci+4,cci+45 GET mgstt PICT '@!' VALID ver_gstt(mgstt,,'*')
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
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
                IF ! EMPTY(mtipo_fx) .AND. ver_faixa(mmes) <> mtipo_fx
                        SKIP
                        LOOP
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
                        DEVPOS(PROW(),52);DEVOUT('Vlr Mens')
                        DEVPOS(PROW(),62);DEVOUT('Desconto')
                        DEVPOS(PROW(),72);DEVOUT(' Receber')
                        DEVPOS(PROW(),82);DEVOUT('Tp.Bolsa')
                        DEVPOS(PROW(),92);DEVOUT('GSTT')
                        DEVPOS(PROW(),98);DEVOUT('Responsavel')
                        imprt('M','N')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        imprt('M','C')
                ENDIF
                IF (! EMPTY(mgstt) .AND. mgstt <> aluno->gstt);
                   .OR. aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(aluno->dt_tranf) .AND. VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                        SKIP
                        LOOP
                ENDIF
                mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),mmes)
                mdesconto := faixa_vlr(ver_faixa(mmes))
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),52);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),62);DEVOUT(TRANSFORM(ROUND(mvlr_men * (mdesconto/100),2),'9,999.99'))
                DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mvlr_men - ROUND((mvlr_men * (mdesconto/100)),2),'9,999.99'))
                DEVPOS(PROW(),86);DEVOUT(ver_faixa(mmes))
                DEVPOS(PROW(),92);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),98);DEVOUT(aluno->responsa)
                mcont_aluno ++
                mtot_men := mtot_men + mvlr_men
                mtot_des := mtot_des + ROUND((mvlr_men * (mdesconto/100)),2)
                mger_aluno ++
                mger_men := mger_men + mvlr_men
                mger_des := mger_des + ROUND((mvlr_men * (mdesconto/100)),2)
                SKIP
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+2,00);DEVOUT('T O T A L    G E R A L')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mger_aluno,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total em Mensalidade: '+TRANSFORM(mger_men,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Bolsas.....: '+TRANSFORM(mger_des,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Receber.....: '+TRANSFORM(mger_men-mger_des,'999,999.99'))
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

