************************************************
* EMISSAO DE CARNES AVULSOS
************************************************

FUNCTION saf5253
***************
LOCAL mprg:='SAF5253'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,minicio,;
      tela,mgst,mmatricula,mop,maluno,mvencimento,mobs

PRIVATE m_parcela:={},mquantd_parc

IF ! ver_nivel(mprg,'EMISSAO DE CARNES AVULSO (MENSALIDADES)','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)
mtraco := REPLI('=',80)
lci := 06
cci := 05
lba := 12
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mop := 0
        mgstt := SPACE(4)
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mquantd_parc := 0
        mindex := 0
        mmes := SPACE(2)
        mquantd = 1
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite a MATRICULA : ')
        DEVPOS(lci+2,cci+1);DEVOUT('Digite a Parcela...:    GSTT:')
        DEVPOS(lci+3,cci+1);DEVOUT('Vencimento.........: ')
        DEVPOS(lci+4,cci+1);DEVOUT('Valor da Parcela R$: ')
        DEVPOS(lci+5,cci+1);DEVOUT('Observacao.........: ')
        @ lci+1,cci+22 GET mmatricula PICT '@!'
        @ lci+1,cci+29 GET maluno PICT '@!' WHEN EMPTY(mmatricula)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF mmatricula <> 'NOVATO'
                IF ! EMPTY(mmatricula)
                        ORDSETFOCUS(4)
                        IF ! aluno->(DBSEEK(mmatricula))
                                atencao('Matricula nao encontrada no arquivo')
                                LOOP
                        ENDIF
                ELSEIF ! EMPTY(maluno)
                        ORDSETFOCUS(3)
                        IF ! aluno->(DBSEEK(RTRIM(maluno)))
                                atencao('Aluno nao encontrada no arquivo')
                                LOOP
                        ENDIF
                        mmatricula := f7_aluno()
                ELSE
                        LOOP
                ENDIF
                setcor(3)
                DEVPOS(lci+1,cci+22);DEVOUT(aluno->matricula)
                DEVPOS(lci+1,cci+29);DEVOUT(aluno->aluno)
                DEVPOS(lci+1,cci+31);DEVOUT(aluno->gstt)
                setcor(1)
                maluno := aluno->aluno
                mgstt  := aluno->gstt
        ELSE
                @ lci+1,cci+29 GET maluno PICT '@!'
                READ
        ENDIF
        mvlr_men := 0
        mvencimento := CTOD('  /  /  ')
        mobs := SPACE(15)
        @ lci+2,cci+22 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+2,cci+31 GET mgstt PICT '@!'              //WHEN mmatricula = 'NOVATO'
        @ lci+3,cci+22 GET mvencimento
        @ lci+4,cci+22 GET mvlr_men PICT '99,999.99'
        @ lci+5,cci+22 GET mobs
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
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
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
*       SET PRINT TO 'LPT1'
        mgst := SUBSTR(mgstt,1,2)+SUBSTR(mgstt,4,1)
        IF maut = 'NOBREGA'
                imprt('M','N',1)
                DEVPOS(PROW(),51);DEVOUT(mobs)
                DEVPOS(PROW(),76);DEVOUT(mobs)
                DEVPOS(PROW()+4,00);DEVOUT(mmatricula)
                DEVPOS(PROW(),10);DEVOUT(maluno)
                DEVPOS(PROW(),51);DEVOUT(mmatricula)
                DEVPOS(PROW(),61);DEVOUT(mgst)
                DEVPOS(PROW(),76);DEVOUT(mmatricula)
                DEVPOS(PROW(),86);DEVOUT(mgst)
                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(mgstt,1,1))
                DEVPOS(PROW(),10);DEVOUT(SUBSTR(mgstt,2,1))
                DEVPOS(PROW(),16);DEVOUT(SUBSTR(mgstt,3,1))
                DEVPOS(PROW(),24);DEVOUT(SUBSTR(mgstt,4,1))
                DEVPOS(PROW(),33);DEVOUT(mobs)
                DEVPOS(PROW(),51);DEVOUT(mmes)
                DEVPOS(PROW(),57);DEVOUT(mvencimento)
                DEVPOS(PROW(),76);DEVOUT(mmes)
                DEVPOS(PROW(),81);DEVOUT(mvencimento)
                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW()+2,25);DEVOUT(mmes)
                DEVPOS(PROW(),31);DEVOUT(mvencimento)
                DEVPOS(PROW()+2,00);DEVOUT('                             ')
                DEVPOS(PROW()+6,51);DEVOUT(maluno)
                DEVPOS(PROW()+4,00);DEVOUT(' ')
        ELSE
                imprt('M','N')
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                DEVPOS(PROW(),40);DEVOUT(memp+resa)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                DEVPOS(PROW(),69);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+mend_firm)
                DEVPOS(PROW(),69);DEVOUT('Endereco: '+mend_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Bairro..: '+mcid_firm)
                DEVPOS(PROW(),69);DEVOUT('Bairro..: '+mcid_firm)
                DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+mmatricula)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+maluno)
                DEVPOS(PROW(),69);DEVOUT('Aluno: '+mmatricula)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+maluno)
                DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+mgstt)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+mmes+'a.Parcela')
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+DTOC(mvencimento))
                DEVPOS(PROW(),69);DEVOUT('GSTT: '+mgstt)
                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+mmes+'a.Parcela')
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+DTOC(mvencimento))
                DEVPOS(PROW()+1,00);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),69);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW()+2,00);DEVOUT('Data Pagamento:......./......./.......')
                DEVPOS(PROW(),69);DEVOUT('Data Pagamento:......./......./.......')
                DEVPOS(PROW()+2,00);DEVOUT('TESOURARIA DO '+memp+resa)
                DEVPOS(PROW(),69);DEVOUT('TESOURARIA DO '+memp+resa)
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+2,00);DEVOUT(REPLI('-',80))
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        LOOP

ENDDO
RETURN NIL
**************************** F I M **************************

