***********************************************
* GERA ARQUIVO DE REMESSA DE PAGAMENTO DO BANCO
***********************************************

FUNCTION safreme(mrel)
******************

LOCAL tela,opcao,lci,cci,lba,cba,marq,msele,morde,mmatricula,mparcela,mvalor,mvlr_reme

LOCAL mpag,mtit,mtipo,mtraco,mtot_baixa,mtot_parc,mtot_multa,mtot_geral,mfaixa,m_critica:={},i

lci := 10
cci := 03
lba := 13
cba := 78
tela := SAVESCREEN(00,00,24,79)
mtraco := REPLI('=',80)
WHILE .T.
        marq := 'C:\CAIXA\NOBREGA\RETORNO\'+SPACE(25)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Caminho do Arquivo (com extensao)..:')
        DEVPOS(lci+2,cci+1);DEVOUT('Matricula que esta sendo processada:')
        DEVPOS(lci+2,cci+46);DEVOUT('Parcela:')
        DEVPOS(lci+2,cci+58);DEVOUT('Valor:')
        @ lci+1,cci+38 GET marq PICT '@S30!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        marq := RTRIM(marq)
        IF ! FILE((marq))
                atencao('Nao foi possivel encontra este ARQUIVO')
                LOOP
        ENDIF
        mensagem('Confirma a operacao [S/n]:')
        opcao := 'S'
        @ 23,COL()+2 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        ****************************************
        CLOSE ALL
        IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
        IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
        IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
        IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
        IF ! abriarq('remessa','reme','E');RETURN NIL;ENDIF
        ****************************************
        ***************
        SELE('reme')
        msele := SELE()
        ***************
        ZAP
        APPEND FROM (marq) DELIMITED WITH BLANK
        GO TOP
        DELE FOR SUBSTR(linha1,1,1) = '0' .OR. SUBSTR(linha1,1,1) = '9'
        PACK
        GO TOP
        IF mrel = NIL
                WHILE ! EOF()
                        mvalor := 0
                        mparcela := SUBSTR(reme->linha2,9,2)
                        mmatricula := SUBSTR(reme->linha2,3,6)
                        mvlr_reme := VAL(reme->linha6)/10000000000
                        setcor(3)
                        DEVPOS(lci+2,cci+38);DEVOUT(mmatricula)
                        DEVPOS(lci+2,cci+55);DEVOUT(mparcela)
                        DEVPOS(lci+2,cci+65);DEVOUTPICT(mvlr_reme,'9,999.99')
                        setcor(1)
                        IF ! mat_parc(mmatricula,mparcela)
                                atencao('A Parcela: '+mparcela+'da matricula '+mmatricula+' ja foi dado baixa esta rotina de Criticas, Verifique o arquivo de REMESSA do Banco se ja foi processado')
                                AADD(m_critica,'Matricula: '+mmatricula+' Parcela: '+mparcela+' Valor:'+ TRANSFORM(mvlr_reme,'9,999.99')+' Pagamento em duplicidade')
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_aluno(mmatricula)
                                atencao('Esta matricula: '+SUBSTR(reme->linha2,3,6)+' nao foi encontrada verificar este arquivo de REMESSA')
                                AADD(m_critica,'Matricula: '+mmatricula+' Parcela: '+mparcela+' Valor:'+ TRANSFORM(mvlr_reme,'9,999.99')+' Matricula nao Encontrada')
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
                                atencao('Tabela nao encontrada')
                                AADD(m_critica,'Matricula: '+mmatricula+' Parcela: '+mparcela+' GSTT:'+maluno->gstt+' Valor:'+ TRANSFORM(mvlr_reme,'9,999.99')+' Nao foi encontrado na tabela o GSTT')
                                SKIP
                                LOOP
                        ENDIF
                        IF mparcela = '01'
                                mfaixa := aluno->faixa_1
                        ELSEIF mparcela = '02'
                                mfaixa := aluno->faixa_2
                        ELSEIF mparcela = '03'
                                mfaixa := aluno->faixa_3
                        ELSEIF mparcela = '04'
                                mfaixa := aluno->faixa_4
                        ELSEIF mparcela = '05'
                                mfaixa := aluno->faixa_5
                        ELSEIF mparcela = '06'
                                mfaixa := aluno->faixa_6
                        ELSEIF mparcela = '07'
                                mfaixa := aluno->faixa_7
                        ELSEIF mparcela = '08'
                                mfaixa := aluno->faixa_8
                        ELSEIF mparcela = '09'
                                mfaixa := aluno->faixa_9
                        ELSEIF mparcela = '10'
                                mfaixa := aluno->faixa_10
                        ELSEIF mparcela = '11'
                                mfaixa := aluno->faixa_11
                        ELSEIF mparcela = '12'
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ! ver_fx(mfaixa)
                                atencao('Faixa nao encontrada')
                                AADD(m_critica,'Matricula: '+mmatricula+' Parcela: '+mparcela+' Faixa:'+mfaixa+' Valor:'+ TRANSFORM(mvlr_reme,'9,999.99')+' Nao foi encontrado esta Faixa')
                                SKIP
                                LOOP
                        ENDIF
                        IF fx->desconto = 0
                                mvalor := tabmen->valor
                        ELSE
                                mvalor := tabmen->valor - (tabmen->valor * (fx->desconto/100))
                        ENDIF
/*
                        *****************
                        SELE('cdmen');ORDSETFOCUS(1)
                        *****************
                        IF ! ADIREG()
                                atencao('Nao foi possivel criar o registro')
                                LOOP
                        ENDIF
                        cdmen->gstt := aluno->gstt
                        cdmen->matricula := mmatricula
                        cdmen->bolsa := mfaixa
                        cdmen->parcela := mparcela
                        cdmen->vlr_parc := mvalor
                        cdmen->data_pro := DATE()
                        cdmen->data_pag := CTOD(SUBSTR(reme->linha4,5,2)+'/'+SUBSTR(reme->linha4,7,2)+'/'+SUBSTR(reme->linha4,9,2))
                        cdmen->valor := mvlr_reme
                        cdmen->multa := mvlr_reme-mvalor
                        cdmen->flag := 'B'
                        DBUNLOCK()
                        DBCOMMIT()
*/
                        SELE(msele)
                        SKIP
                ENDDO
                DBUNLOCKALL()
                DBCOMMITALL()
        ENDIF
        mensagem('Deseja Imprimir o processamento [S/N]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        mensagem('Aguarde o final da impressao')
        SET DEVI TO PRINT
        * SET PRINT TO LPT1
        mpag := 1
        mtit := 'Relacao de Baixa de mensalidades'
        mtipo := 'Arquivo de Remessa: '+marq
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C',1)
        DEVPOS(PROW(),00);DEVOUT('Matricula')
        DEVPOS(PROW(),11);DEVOUT('Nome')
        DEVPOS(PROW(),53);DEVOUT('GSTT')
        DEVPOS(PROW(),58);DEVOUT('Dta.Proc.')
        DEVPOS(PROW(),70);DEVOUT('Dta.Pag. ')
        DEVPOS(PROW(),82);DEVOUT('Parcela')
        DEVPOS(PROW(),91);DEVOUT('Vlr.Parc.')
        DEVPOS(PROW(),102);DEVOUT('   Multa')
        DEVPOS(PROW(),112);DEVOUT('Vlr.Pag.')
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        imprt('M','C')
        GO TOP
        mtot_baixa := 0
        mtot_parc := 0
        mtot_multa := 0
        mtot_geral := 0
        WHILE ! EOF()
                mvalor := 0
                mparcela := SUBSTR(reme->linha2,9,2)
                mmatricula := SUBSTR(reme->linha2,3,6)
                mvlr_reme := VAL(reme->linha6)/10000000000
                IF ! ver_aluno(mmatricula)
*                       atencao('Esta matricula: '+SUBSTR(reme->linha2,3,6)+' nao foi encontrada verificar este arquivo de REMESSA')
                        SKIP
                ENDIF
                IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
                        LOOP
                ENDIF
                IF mparcela = '01'
                        mfaixa := aluno->faixa_1
                ELSEIF mparcela = '02'
                        mfaixa := aluno->faixa_2
                ELSEIF mparcela = '03'
                        mfaixa := aluno->faixa_3
                ELSEIF mparcela = '04'
                        mfaixa := aluno->faixa_4
                ELSEIF mparcela = '05'
                        mfaixa := aluno->faixa_5
                ELSEIF mparcela = '06'
                        mfaixa := aluno->faixa_6
                ELSEIF mparcela = '07'
                        mfaixa := aluno->faixa_7
                ELSEIF mparcela = '08'
                        mfaixa := aluno->faixa_8
                ELSEIF mparcela = '09'
                        mfaixa := aluno->faixa_9
                ELSEIF mparcela = '10'
                        mfaixa := aluno->faixa_10
                ELSEIF mparcela = '11'
                        mfaixa := aluno->faixa_11
                ELSEIF mparcela = '12'
                        mfaixa := aluno->faixa_12
                ENDIF
                IF ! ver_fx(mfaixa)
                        LOOP
                ENDIF
                IF fx->desconto = 0
                        mvalor := tabmen->valor
                ELSE
                        mvalor := tabmen->valor - (tabmen->valor * (fx->desconto/100))
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(mmatricula)
                DEVPOS(PROW(),11);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),53);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),58);DEVOUT(DATE())
                DEVPOS(PROW(),70);DEVOUT(CTOD(SUBSTR(reme->linha4,5,2)+'/'+SUBSTR(reme->linha4,7,2)+'/'+SUBSTR(reme->linha4,9,2)))
                DEVPOS(PROW(),84);DEVOUT(mparcela)
                DEVPOS(PROW(),91);DEVOUTPICT(mvalor,'9,999.99')
                DEVPOS(PROW(),102);DEVOUTPICT(mvlr_reme-mvalor,'9,999.99')
                DEVPOS(PROW(),112);DEVOUTPICT(mvlr_reme,'9,999.99')
                mtot_baixa ++
                mtot_parc := mtot_parc + mvalor
                mtot_multa := mtot_multa + (mvlr_reme-mvalor)
                mtot_geral := mtot_geral + mvlr_reme
                SKIP
                IF PROW() > 57
                        EJECT
                        mpag++
                        cabecalho(mpag,mtit)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT('Matricula')
                        DEVPOS(PROW(),11);DEVOUT('Nome')
                        DEVPOS(PROW(),53);DEVOUT('GSTT')
                        DEVPOS(PROW(),58);DEVOUT('Dta.Proc.')
                        DEVPOS(PROW(),70);DEVOUT('Dta.Pag. ')
                        DEVPOS(PROW(),82);DEVOUT('Parcela')
                        DEVPOS(PROW(),91);DEVOUT('Vlr.Parc.')
                        DEVPOS(PROW(),102);DEVOUT('   Multa')
                        DEVPOS(PROW(),112);DEVOUT('Vlr.Pag.')
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                        imprt('M','C')
                ENDIF
        ENDDO
        IF PROW() > 57
                EJECT
                mpag++
                cabecalho(mpag,mtit)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Matricula')
                DEVPOS(PROW(),11);DEVOUT('Nome')
                DEVPOS(PROW(),53);DEVOUT('GSTT')
                DEVPOS(PROW(),58);DEVOUT('Dta.Proc.')
                DEVPOS(PROW(),70);DEVOUT('Dta.Pag. ')
                DEVPOS(PROW(),82);DEVOUT('Parcela')
                DEVPOS(PROW(),91);DEVOUT('Vlr.Parc.')
                DEVPOS(PROW(),102);DEVOUT('   Multa')
                DEVPOS(PROW(),112);DEVOUT('Vlr.Pag.')
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                imprt('M','C')
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW()+1,00);DEVOUT('********************** RESUMO GERAL *********************')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidades de BAIXAS:   '+TRANSFORM(mtot_baixa,'99,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total em Parcelas R$.: '+TRANSFORM(mtot_parc,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total em Multas...R$.: '+TRANSFORM(mtot_multa,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('                       -------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total em Geral....R$.: '+TRANSFORM(mtot_geral,'9,999,999.99'))
        IF LEN(m_critica) > 0
                EJECT
                mpag++
                cabecalho(mpag,mtit)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Matricula')
                DEVPOS(PROW(),11);DEVOUT('Nome')
                DEVPOS(PROW(),53);DEVOUT('GSTT')
                DEVPOS(PROW(),58);DEVOUT('Dta.Proc.')
                DEVPOS(PROW(),70);DEVOUT('Dta.Pag. ')
                DEVPOS(PROW(),82);DEVOUT('Parcela')
                DEVPOS(PROW(),91);DEVOUT('Vlr.Parc.')
                DEVPOS(PROW(),102);DEVOUT('   Multa')
                DEVPOS(PROW(),112);DEVOUT('Vlr.Pag.')
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                imprt('M','C')
                imprt('M','N',1)
                DEVPOS(PROW()+1,00);DEVOUT('********************** RESUMO DAS CRITICAS **************')
                i := 0
                FOR i = 1 TO LEN(m_critica)
                        DEVPOS(PROW()+1,00);DEVOUT(m_critica[i])
                NEXT
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
RETURN NIL
