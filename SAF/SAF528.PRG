**********************************************
* DECLARACAO DE PAGAMENTO DE MENSALIDADE P/IR
**********************************************

FUNCTION saf528
**************
LOCAL mprg:='SAF528'
LOCAL tela,lci,cci,lba,cba,opcao,mmatricual,mnome,mtotal,mobs1mobs2,mobs3,i,;
      m_matriz:={},mtotal_parc:=0

IF ! ver_nivel(mprg,'DECLARACAO DE PAGAMENTOS MENSALIDADES P/IR','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)
lci := 03
cci := 02
lba := 21
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
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula..:')
        DEVPOS(lci+2,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+3,cci+1);DEVOUT('Observacao.:')
        DEVPOS(lci+6,cci+1);DEVOUT('Parcela....:')
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
        *****************
        SELE('cdmen')
        ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! cdmen->(DBSEEK(mmatricula))
                atencao('Nao existe nenhum pagamento deste aluno')
                LOOP
        ENDIF
        ASIZE(m_matriz,0)
        i := 0
        FOR i = 1 TO 12
                IF ! mat_parc(mmatricula,STRZERO(i,2),'*','*','ME') .OR. EMPTY(cdmen->data_pag)
                        IF i = 1
                                AADD(m_matriz,{'Janeiro/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 2
                                AADD(m_matriz,{'Fevereiro/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 3
                                AADD(m_matriz,{'Marco/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 4
                                AADD(m_matriz,{'Abril/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 5
                                AADD(m_matriz,{'Maio/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 6
                                AADD(m_matriz,{'Junho/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 7
                                AADD(m_matriz,{'Julho/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 8
                                AADD(m_matriz,{'Agosto/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 9
                                AADD(m_matriz,{'Setembro/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 10
                                AADD(m_matriz,{'Outubro/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 11
                                AADD(m_matriz,{'Novembro/'+mdiretorio,0,SPACE(8),0})
                        ELSEIF i = 12
                                AADD(m_matriz,{'Dezembro/'+mdiretorio,0,SPACE(8),0})
                        ENDIF
                ELSE
                        IF i = 1
                                AADD(m_matriz,{'Janeiro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 2
                                AADD(m_matriz,{'Fevereiro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 3
                                AADD(m_matriz,{'Marco/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 4
                                AADD(m_matriz,{'Abril/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 5
                                AADD(m_matriz,{'Maio/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 6
                                AADD(m_matriz,{'Junho/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 7
                                AADD(m_matriz,{'Julho/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 8
                                AADD(m_matriz,{'Agosto/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 9
                                AADD(m_matriz,{'Setembro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 10
                                AADD(m_matriz,{'Outubro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 11
                                AADD(m_matriz,{'Novembro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ELSEIF i = 12
                                AADD(m_matriz,{'Dezembro/'+mdiretorio,cdmen->vlr_parc,cdmen->data_pag,cdmen->valor})
                        ENDIF
                ENDIF
        NEXT
        DEVPOS(lci+6,cci+14);DEVOUT(m_matriz[1,1]+' :')
        DEVPOS(lci+7,cci+14);DEVOUT(m_matriz[2,1]+' :')
        DEVPOS(lci+8,cci+14);DEVOUT(m_matriz[3,1]+' :')
        DEVPOS(lci+9,cci+14);DEVOUT(m_matriz[4,1]+' :')
        DEVPOS(lci+10,cci+14);DEVOUT(m_matriz[5,1]+' :')
        DEVPOS(lci+11,cci+14);DEVOUT(m_matriz[6,1]+' :')
        DEVPOS(lci+12,cci+14);DEVOUT(m_matriz[7,1]+' :')
        DEVPOS(lci+13,cci+14);DEVOUT(m_matriz[8,1]+' :')
        DEVPOS(lci+14,cci+14);DEVOUT(m_matriz[9,1]+' :')
        DEVPOS(lci+15,cci+14);DEVOUT(m_matriz[10,1]+' :')
        DEVPOS(lci+16,cci+14);DEVOUT(m_matriz[11,1]+' :')
        DEVPOS(lci+17,cci+14);DEVOUT(m_matriz[12,1]+' :')
        @ lci+6,cci+31 GET m_matriz[1,2] PICT '9,999.99'
        @ lci+7,cci+31 GET m_matriz[2,2] PICT '9,999.99'
        @ lci+8,cci+31 GET m_matriz[3,2] PICT '9,999.99'
        @ lci+9,cci+31 GET m_matriz[4,2] PICT '9,999.99'
        @ lci+10,cci+31 GET m_matriz[5,2] PICT '9,999.99'
        @ lci+11,cci+31 GET m_matriz[6,2] PICT '9,999.99'
        @ lci+12,cci+31 GET m_matriz[7,2] PICT '9,999.99'
        @ lci+13,cci+31 GET m_matriz[8,2] PICT '9,999.99'
        @ lci+14,cci+31 GET m_matriz[9,2] PICT '9,999.99'
        @ lci+15,cci+31 GET m_matriz[10,2] PICT '9,999.99'
        @ lci+16,cci+31 GET m_matriz[11,2] PICT '9,999.99'
        @ lci+17,cci+31 GET m_matriz[12,2] PICT '9,999.99'
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
        mensagem('Aguarde o final da impressao...')
        SET DEVI TO PRINT
        * * SET PRINT TO LPT1
        DEVPOS(PROW(),00);DEVOUT(CHR(27)+'W'+CHR(1))
        DEVPOS(PROW(),00);DEVOUT(memp+resa)
        DEVPOS(PROW()+1,00);DEVOUT(CHR(27)+'W'+CHR(0))
        DEVPOS(PROW(),00);DEVOUT(mend_firm+' - '+mcid_firm+' - '+mcgc_firm)
        DEVPOS(PROW()+4,00);DEVOUT(PADC('D E C L A R A C A O',80))
        DEVPOS(PROW()+4,00);DEVOUT('    Declaro para fins de prova perante a Delegacia Regional do Imposto de renda,')
        DEVPOS(PROW()+2,00);DEVOUT('que o Sr.(a) '+RTRIM(mnome)+REPLI('_',40-LEN(RTRIM(mnome)))+', pagou por seu dependente,')
        DEVPOS(PROW()+2,00);DEVOUT(RTRIM(aluno->aluno)+REPLI('_',40-LEN(RTRIM(aluno->aluno)))+','+aluno->gstt+' ao '+memp+resa+', Durante o ano')
        DEVPOS(PROW()+2,00);DEVOUT(mdiretorio+', prestacoes diversas conforme demonstrativo abaixo:')
        DEVPOS(PROW()+3,00);DEVOUT('Mes e Ano      Valor Mens.   Enc./Desc.R$  Valor Pago R$  Data de Pag.')
        DEVPOS(PROW()+1,00);DEVOUT('-------------- ------------- ------------- -------------  ------------')
        mtotal := 0
        mtotal_parc := 0
        i := 0
        FOR i = 1 TO 12
                IF m_matriz[i,2] = 0
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_matriz[i,1])
                DEVPOS(PROW(),20);DEVOUTPICT(m_matriz[i,2],'9,999.99')
                DEVPOS(PROW(),PCOL()+6);DEVOUTPICT((m_matriz[i,4]-m_matriz[i,2]),'9,999.99')
/*
                IF m_matriz[i,2] < m_matriz[i,4]
                        DEVPOS(PROW(),PCOL()+6);DEVOUTPICT((m_matriz[i,2]-m_matriz[i,4])*-1,'9,999.99')
                ELSE
                        DEVPOS(PROW(),PCOL()+6);DEVOUTPICT((m_matriz[i,2]-m_matriz[i,4]),'9,999.99')
                ENDIF
*/
                DEVPOS(PROW(),PCOL()+6);DEVOUTPICT(m_matriz[i,4],'9,999.99')
                DEVPOS(PROW(),PCOL()+6);DEVOUT(m_matriz[i,3])
                mtotal := mtotal + m_matriz[i,4]
                mtotal_parc := mtotal_parc + m_matriz[i,2]
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT('               ------------- ------------- -------------')
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL.....R$:       '+TRANSFORM(mtotal_parc,'9,999.99'))
        DEVPOS(PROW(),PCOL()+6);DEVOUT(TRANSFORM((mtotal - mtotal_parc),'9,999.99'))
/*
        IF mtotal_parc < mtotal
                DEVPOS(PROW(),PCOL()+6);DEVOUT(TRANSFORM((mtotal_parc - mtotal)*-1,'9,999.99'))
        ELSE
                DEVPOS(PROW(),PCOL()+6);DEVOUT(TRANSFORM(mtotal_parc - mtotal,'9,999.99'))
        ENDIF
*/
        DEVPOS(PROW(),PCOL()+6);DEVOUT(TRANSFORM(mtotal,'9,999.99'))
        DEVPOS(38,00);DEVOUT('OBS: ')
        DEVPOS(38,05);DEVOUT(mobs1)
        DEVPOS(39,05);DEVOUT(mobs2)
        DEVPOS(40,05);DEVOUT(mobs3)

        DEVPOS(45,00);DEVOUT(PADC('Recife,_____ de __________________ de ______',80))
        DEVPOS(50,00);DEVOUT(PADC('____________________________________________',80))
        DEVPOS(51,00);DEVOUT(PADC('           Direcao Administrativa           ',80))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
RETURN NIL



