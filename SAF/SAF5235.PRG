************************************************
* RELATORIO DE ALUNOS BOLSISTA ENDERECO ANUAL
************************************************

FUNCTION saf5235
***************
LOCAL mprg:='SAF5235'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mtipo_fx,mordem,mtipo_vlr,mgstt:=SPACE(4),mvlr_tot:=0,mvlr_des:=0,;
      lci,cci,lba,cba,mbol1,mbol2,mbol3,mbol4,mbol5,mbol6,mbol7,mbol8,mbol9,mbol10,mbol11,mbol12,;
      mbol13,mbol14,mbol15,mbol16,mbol17,mbol18,mbol19,mbol20

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS BOLSISTAS E ENDERECO ANUAL','125')
        RETURN NIL
ENDIF
lci := 10
cci := 05
lba := 18
cba := 78
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
        mgstt = '    '
        mbol1 := ' '
        mbol2 := ' '
        mbol3 := ' '
        mbol4 := ' '
        mbol5 := ' '
        mbol6 := ' '
        mbol7 := ' '
        mbol8 := ' '
        mbol9 := ' '
        mbol10:= ' '
        mbol11:= ' '
        mbol12:= ' '
        mbol13:= ' '
        mbol14:= ' '
        mbol15:= ' '
        mbol16:= ' '
        mbol17:= ' '
        mbol18:= ' '
        mbol19:= ' '
        mbol20:= ' '
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
        botao(lci,cci,lba,cba,,' Relacao de Bolsista Anual ')
        DEVPOS(lci+1,cci+1);DEVOUT('Informe a Bolsa ou ENTER p/todas.............:')
        DEVPOS(lci+2,cci+1);DEVOUT('Ordem [1]Resp. [2]GSTT [3]Aluno [4]Matricula.:')
        DEVPOS(lci+3,cci+1);DEVOUT('Deseja imprimir o valor da Bolsa.............:')
        DEVPOS(lci+4,cci+1);DEVOUT('Digite o GSTT ou deixe em branco p/todos.....:')
        DEVPOS(lci+5,cci+1);DEVOUT('Digite o Mes  ou deixe em branco p/todos.....:')
        DEVPOS(lci+6,cci+1);DEVOUT('Digite as Bolsa que deseja Excluir da Relacao:')
        @ lci+1,cci+48 GET mtipo_fx
        @ lci+2,cci+48 GET mordem PICT '9' RANGE 1,4
        @ lci+3,cci+48 GET mtipo_vlr PICT '@!' VALID mtipo_vlr $ 'S,N'
        @ lci+4,cci+48 GET mgstt PICT '@!' VALID ver_gstt(mgstt,,'*')
        @ lci+5,cci+48 GET mmes PICT '@!' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12,  '
        @ lci+6,cci+48 GET mbol1 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol2 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol3 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol4 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol5 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol6 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol7 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol8 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol9 WHEN EMPTY(mtipo_fx)
        @ lci+6,COL()+1 GET mbol10 WHEN EMPTY(mtipo_fx)
        @ lci+7,cci+48 GET mbol11 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol12 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol13 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol14 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol15 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol16 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol17 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol18 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol19 WHEN EMPTY(mtipo_fx)
        @ lci+7,COL()+1 GET mbol20 WHEN EMPTY(mtipo_fx)
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
        IF ! EMPTY(mtipo_fx)
                mtipo := 'Valor da Bolsa em percentual: '+TRANSFORM(faixa_vlr(mtipo_fx)*100,'999,99')+'%'
        ENDIF
*       mfaixa := ver_faixa(mmes)
        mpag := 0
        WHILE ! EOF()
                IF (! EMPTY(mgstt) .AND. mgstt <> aluno->gstt .OR. EMPTY(aluno->contrato));
                   .OR. (! EMPTY(aluno->dt_tranf) .AND. VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2)));
                   .OR. aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                i := 0
                mvlr_tot := 0
                mvlr_des := 0
                FOR i = 1 TO 12
                        IF ! EMPTY(mmes)
                                IF VAL(mmes) <> i
                                        LOOP
                                ENDIF
                        ENDIF
                        IF (i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2)));
                           .OR. (! EMPTY(aluno->dt_tranf) .AND. VAL(STRZERO(i,2)) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2)))
                                LOOP
                        ENDIF
                        IF ! EMPTY(mtipo_fx)
                                IF ver_faixa(STRZERO(i,2)) <> mtipo_fx
                                        LOOP
                                ENDIF
                                mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),STRZERO(i,2))
                                mdesconto := faixa_vlr(ver_faixa(STRZERO(i,2)))
                                IF mdesconto = 0
                                        LOOP
                                ENDIF
                                mvlr_tot := mvlr_tot + (mvlr_men - ROUND((mvlr_men * (mdesconto/100)),2))
                                mvlr_des := mvlr_des + ROUND((mvlr_men * (mdesconto/100)),2)
                                mtot_men := mtot_men + mvlr_men
                        ELSE
*                               IF ver_faixa(STRZERO(i,2)) = 'V' .OR. ver_faixa(STRZERO(i,2)) = 'Z'
*                                       LOOP
*                               ENDIF
                                IF ! EMPTY(mbol1) .AND. ver_faixa(STRZERO(i,2)) = mbol1
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol2) .AND. ver_faixa(STRZERO(i,2)) = mbol2
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol3) .AND. ver_faixa(STRZERO(i,2)) = mbol3
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol4) .AND. ver_faixa(STRZERO(i,2)) = mbol4
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol5) .AND. ver_faixa(STRZERO(i,2)) = mbol5
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol6) .AND. ver_faixa(STRZERO(i,2)) = mbol6
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol7) .AND. ver_faixa(STRZERO(i,2)) = mbol7
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol8) .AND. ver_faixa(STRZERO(i,2)) = mbol8
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol9) .AND. ver_faixa(STRZERO(i,2)) = mbol9
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol10) .AND. ver_faixa(STRZERO(i,2)) = mbol10
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol11) .AND. ver_faixa(STRZERO(i,2)) = mbol11
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol12) .AND. ver_faixa(STRZERO(i,2)) = mbol12
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol13) .AND. ver_faixa(STRZERO(i,2)) = mbol13
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol14) .AND. ver_faixa(STRZERO(i,2)) = mbol14
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol15) .AND. ver_faixa(STRZERO(i,2)) = mbol15
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol16) .AND. ver_faixa(STRZERO(i,2)) = mbol16
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol17) .AND. ver_faixa(STRZERO(i,2)) = mbol17
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol18) .AND. ver_faixa(STRZERO(i,2)) = mbol18
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol19) .AND. ver_faixa(STRZERO(i,2)) = mbol19
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mbol20) .AND. ver_faixa(STRZERO(i,2)) = mbol20
                                        LOOP
                                ENDIF
                                mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),STRZERO(i,2))
                                mdesconto := faixa_vlr(ver_faixa(STRZERO(i,2)))
                                IF mdesconto = 0
                                        LOOP
                                ENDIF
                                mvlr_tot := mvlr_tot + (mvlr_men - ROUND((mvlr_men * (mdesconto/100)),2))
                                mvlr_des := mvlr_des + ROUND((mvlr_men * (mdesconto/100)),2)
                                mtot_men := mtot_men + mvlr_men
                        ENDIF
                NEXT
                IF mvlr_des = 0
                        SKIP
                        LOOP
                ENDIF
                IF PROW() > 57 .OR. mpag = 0
                        IF mpag > 0
                                EJECT
                        ENDIF
                        mpag := mpag + 1
                        mtit := 'Relacao dos Alunos Bolsista do ANO: '+mdiretorio
                        cabecalho(mpag,mtit,mtipo,mprg)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT(' Matric.')
                        DEVPOS(PROW(),10);DEVOUT('Aluno')
                        IF mtipo_vlr = 'S'
                                DEVPOS(PROW(),49);DEVOUT('  Valor Ano')
                        ENDIF
                        DEVPOS(PROW(),62);DEVOUT('Responsavel')
                        DEVPOS(PROW(),104);DEVOUT('C.P.F.')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),51);DEVOUT(TRANSFORM(mvlr_des,'99,999.99'))
                DEVPOS(PROW(),62);DEVOUT(aluno->responsa)
                DEVPOS(PROW(),104);DEVOUT(aluno->r_cpf)
                DEVPOS(PROW()+1,00);DEVOUT('Filiacao do Aluno: PAI: '+RTRIM(pai)+' - MAE: '+RTRIM(mae))
                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+RTRIM(aluno->r_rua)+', '+RTRIM(r_num)+'- Compl.:'+r_apto+' - '+RTRIM(r_bairro)+' - '+RTRIM(r_cidade)+' - '+r_estado+' - '+r_cep+' - Fone:'+r_fone)
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',130))
                mger_aluno ++
                mger_men := mger_men + mvlr_tot
                SKIP
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+2,00);DEVOUT('T O T A L    G E R A L')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mger_aluno,'999,999'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Geral.......: '+TRANSFORM(mtot_men,'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Concedido...: '+TRANSFORM(mtot_men - mger_men,'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Receber.....: '+TRANSFORM(mger_men,'99,999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('BOLSA_IN.REL')
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************

