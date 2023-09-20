****************************************************
*** SAF412.PRG: Consulta de Parcelas a Receber ALUNO
****************************************************

FUNCTION saf412()
***************
LOCAL mprg:='SAF412'

LOCAL tela,opcao,li,ci,la,ca

LOCAL m_cdmen := {},m_pos := {},mtotal_doc,mcont_doc,point,mvalor

IF ! ver_nivel(mprg,'CONSULTA DE PARCELAS A RECEBER DO ALUNO','15')
        RETURN NIL
ENDIF

li := 04
ci := 02
la := 21
ca := 79

tela := SAVESCREEN(00,00,24,79)
WHILE .T.
        mtotal_doc := 0
        mcont_doc := 0
        ASIZE(m_cdmen,0)
        ASIZE(m_pos,0)
        *************
        SELE('cdmen');ORDSETFOCUS(1)
        *************
        GO TOP
        IF cdmen->(DBSEEK(aluno->matricula))
                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF()
*                       IF ! EMPTY(cdmen->data_pag) .OR. VAL(cdmen->parcela) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                        IF ! EMPTY(cdmen->data_pag)
                                SKIP
                                LOOP
                        ENDIF
*                       IF ! ver_fx(ver_faixa(cdmen->parcela)) .AND. ! EMPTY(cdmen->parcela)
                        mvalor    := cdmen->vlr_parc
                        IF cdmen->tipo = 'ME'
                                IF ! ver_fx(cdmen->bolsa) .AND. ! EMPTY(cdmen->parcela)
                                        LOOP
                                ENDIF
                                mvalor      := 0
                                IF fx->desconto = 0
                                        mvalor    := cdmen->vlr_parc
                                ELSE
                                        mvalor    := cdmen->vlr_parc - (cdmen->vlr_parc * (fx->desconto/100))
                                ENDIF
                        ENDIF
                        IF EMPTY(cdmen->parcela)
                                mvalor    := cdmen->vlr_parc
                        ENDIF
                        IF EMPTY(mvalor)
                                SKIP;LOOP
                        ENDIF
                        AADD(m_cdmen,' '+cdmen->ano+' '+cdmen->tipo+'    '+cdmen->parcela+'          '+DTOC(cdmen->data_venc)+'  '+TRANSFORM(mvalor,'9,999.99')+' '+cdmen->conta+'  '+cdmen->bolsa+'  '+cdmen->operador)
                        AADD(m_pos,RECNO())
                        mcont_doc ++
                        mtotal_doc := mtotal_doc + mvalor
                        SKIP
                ENDDO
                IF LEN(m_cdmen) = 0
                        atencao('Nao existe nenhuma Parcela a Receber')
                        EXIT
                ENDIF
                mensagem('<ESC> p/ Retorna')
                setcor(1)
                botao(li,ci,la,ca,,' Parcelas a Receber ')
                DEVPOS(li+1,ci+02);DEVOUT('ANO')
                DEVPOS(li+1,ci+07);DEVOUT('TP')
                DEVPOS(li+1,ci+11);DEVOUT('Parcela  ')
                DEVPOS(li+1,ci+24);DEVOUT('Vencimento')
                DEVPOS(li+1,ci+38);DEVOUT('Valor')
                DEVPOS(li+1,ci+47);DEVOUT('Bolsa')
                DEVPOS(li+1,ci+53);DEVOUT('Oper.')
                @ li+2,ci+1 TO li+2,ca-1
                @ la-2,ci+1 TO la-2,ca-1
                DEVPOS(la-1,ci+1);DEVOUT('Quantidade:')
                DEVPOS(la-1,ci+20);DEVOUT('Total:')
                setcor(1)
                DEVPOS(la-1,ci+13);DEVOUT(STRZERO(mcont_doc,4))
                DEVPOS(la-1,ci+27);DEVOUT(TRANSFORM(mtotal_doc,'99,999.99'))
                point := ACHOICE(li+3,ci+1,la-3,ca-1,m_cdmen,,,point)
                IF LASTKEY()=27
                        RESTSCREEN(0,00,24,79,tela)
                        EXIT
                ENDIF
        ELSE
                atencao('Nao existe nenhuma Parcela p/ o Aluno')
                EXIT
        ENDIF
ENDDO
******************************* FIM **************************************8

