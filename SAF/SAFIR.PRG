**********************************************
* DECLARACAO DE PAGAMENTO DE MENSALIDADE P/IR
**********************************************

FUNCTION safir
**************
LOCAL mprg:='SAFIR'
LOCAL tela,lci,cci,lba,cba,opcao,mmatricual,mnome,mtotal,mobs1mobs2,mobs3

IF ! ver_nivel(mprg,'DECLARACAO PAGAMENTO MENSALIDADE P/IR','15')
        RETURN NIL
ENDIF
tela := SAVESCREEN(00,00,24,79)
lci := 09
cci := 02
lba := 15
cba := 78
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mmatricula := SPACE(6)
        mnome := SPACE(40)
        mobs1 := SPACE(60)
        mobs2 := SPACE(60)
        mobs3 := SPACE(60)
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula..:')
        DEVPOS(lci+2,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+3,cci+1);DEVOUT('Observacao.:')
        @ lci+1,cci+14 GET mmatricula PICT '999999' VALID ver_aluno(mmatricula)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        mnome := aluno->responsa
        @ lci+2,cci+14 GET mnome PICT '@!'
        @ lci+3,cci+14 GET mobs1 PICT '@!'
        @ lci+4,cci+14 GET mobs2 PICT '@!'
        @ lci+5,cci+14 GET mobs3 PICT '@!'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        *****************
        SELE('cdmen')
        ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! cdmen->(DBSEEK(mmatricula))
                atencao('Nao existe nenhum pagamento deste aluno')
                LOOP
        ENDIF
        mensagem('Aguarde o final da impressao...')
        SET DEVI TO PRINT
        * SET PRINT TO LPT1
        DEVPOS(PROW(),00);DEVOUT(CHR(27)+'W'+CHR(1))
        DEVPOS(PROW(),00);DEVOUT(memp+resa)
        DEVPOS(PROW()+1,00);DEVOUT(CHR(27)+'W'+CHR(0))
        DEVPOS(PROW(),00);DEVOUT(mend_firm+' - '+mcid_firm+' - '+mcgc_firm)
        DEVPOS(PROW()+4,00);DEVOUT(PADC('D E C L A R A C A O',80))
        DEVPOS(PROW()+4,00);DEVOUT('    Declaro para fins de prova perante a Delegacia Regional do Imposto de renda,')
        DEVPOS(PROW()+2,00);DEVOUT('que o Sr.(a) '+RTRIM(mnome)+REPLI('_',40-LEN(RTRIM(mnome)))+', pagou por seu dependente,')
        DEVPOS(PROW()+2,00);DEVOUT(RTRIM(aluno->aluno)+REPLI('_',40-LEN(RTRIM(aluno->aluno)))+','+aluno->gstt+' ao '+memp+resa+', Durante o ano')
        DEVPOS(PROW()+2,00);DEVOUT(mdiretorio+', prestacoes diversas conforme demonstrativo abaixo:')
        DEVPOS(PROW()+3,00);DEVOUT('Mes e Ano      Valor Pago R$')
        DEVPOS(PROW()+1,00);DEVOUT('-------------- -------------')
        mtotal := 0
        WHILE mmatricula = cdmen->matricula .AND. ! EOF()
                IF cdmen->parcela = '01'
                        DEVPOS(PROW()+1,00);DEVOUT('Janeiro/'+mdiretorio)
                ELSEIF cdmen->parcela = '02'
                        DEVPOS(PROW()+1,00);DEVOUT('Fevereiro/'+mdiretorio)
                ELSEIF cdmen->parcela = '03'
                        DEVPOS(PROW()+1,00);DEVOUT('Marco/'+mdiretorio)
                ELSEIF cdmen->parcela = '04'
                        DEVPOS(PROW()+1,00);DEVOUT('Abril/'+mdiretorio)
                ELSEIF cdmen->parcela = '05'
                        DEVPOS(PROW()+1,00);DEVOUT('Maio/'+mdiretorio)
                ELSEIF cdmen->parcela = '06'
                        DEVPOS(PROW()+1,00);DEVOUT('Junho/'+mdiretorio)
                ELSEIF cdmen->parcela = '07'
                        DEVPOS(PROW()+1,00);DEVOUT('Julho/'+mdiretorio)
                ELSEIF cdmen->parcela = '08'
                        DEVPOS(PROW()+1,00);DEVOUT('Agosto/'+mdiretorio)
                ELSEIF cdmen->parcela = '09'
                        DEVPOS(PROW()+1,00);DEVOUT('Setembro/'+mdiretorio)
                ELSEIF cdmen->parcela = '10'
                        DEVPOS(PROW()+1,00);DEVOUT('Outubro/'+mdiretorio)
                ELSEIF cdmen->parcela = '11'
                        DEVPOS(PROW()+1,00);DEVOUT('Novembro/'+mdiretorio)
                ELSEIF cdmen->parcela = '12'
                        DEVPOS(PROW()+1,00);DEVOUT('Dezembro/'+mdiretorio)
                ENDIF
                DEVPOS(PROW(),20);DEVOUTPICT(cdmen->valor,'9,999.99')
                mtotal := mtotal + cdmen->valor
                SKIP
        ENDDO
        DEVPOS(PROW()+1,00);DEVOUT('               -------------')
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL.....R$:       '+TRANSFORM(mtotal,'9,999.99'))
        DEVPOS(38,00);DEVOUT('OBS: ')
        DEVPOS(38,05);DEVOUT(mobs1)
        DEVPOS(39,05);DEVOUT(mobs2)
        DEVPOS(40,05);DEVOUT(mobs3)

        DEVPOS(45,00);DEVOUT(PADC('Recife,_____ de __________________ de 19____',80))
        DEVPOS(50,00);DEVOUT(PADC('____________________________________________',80))
        DEVPOS(51,00);DEVOUT(PADC('                Tesoureiro                  ',80))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
RETURN NIL



