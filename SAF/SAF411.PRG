***************************************************
*** SAF411.PRG: Consulta de Parcelas Pagas do ALUNO
***************************************************

FUNCTION saf411()
***************
LOCAL mprg:='SAF411'

LOCAL tela,opcao,li,ci,la,ca

LOCAL m_cdmen := {},m_pos := {},mtotal_doc,mcont_doc,point

IF ! ver_nivel(mprg,'CONSULTA DE PARCELAS PAGAS DO ALUNO','15')
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
        SELE('cdmen');ORDSETFOCUS(7)
        *************
        GO TOP
        IF cdmen->(DBSEEK(aluno->matricula))
                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF()
*                       ver_fx(cdmen->bolsa)
*                       IF EMPTY(cdmen->data_pag) .AND. fx->desconto < 100
                        IF EMPTY(cdmen->data_pag) .AND. cdmen->desc_bolsa < 100
                                SKIP
                                LOOP
                        ENDIF
                        AADD(m_cdmen,' '+cdmen->ano+' '+cdmen->tipo+'    '+cdmen->parcela+'          '+DTOC(cdmen->data_venc)+'     '+DTOC(cdmen->data_pag)+'   '+TRANSFORM(cdmen->valor,'9,999.99')+' '+cdmen->conta+'    '+cdmen->flag+'       '+cdmen->operador)
                        AADD(m_pos,RECNO())
                        mcont_doc ++
                        mtotal_doc := mtotal_doc + cdmen->valor
                        SKIP
                ENDDO
                IF LEN(m_cdmen) = 0
                        atencao('Nao existe nenhuma Parcela Paga')
                        EXIT
                ENDIF
                mensagem('<ESC> p/ Retorna')
                setcor(1)
                botao(li,ci,la,ca,,' Parcelas Pagas ')
                DEVPOS(li+1,ci+02);DEVOUT('ANO')
                DEVPOS(li+1,ci+07);DEVOUT('TP')
                DEVPOS(li+1,ci+11);DEVOUT('Parcela  ')
                DEVPOS(li+1,ci+24);DEVOUT('Vencimento')
                DEVPOS(li+1,ci+37);DEVOUT('Pagamento')
                DEVPOS(li+1,ci+49);DEVOUT('Vlr.Pago')
                DEVPOS(li+1,ci+59);DEVOUT('Local Pgto.')
                DEVPOS(li+1,ci+71);DEVOUT('Oper.')
                @ li+2,ci+1 TO li+2,ca-1
                @ la-2,ci+1 TO la-2,ca-1
                DEVPOS(la-1,ci+1);DEVOUT('Quantidade:')
                DEVPOS(la-1,ci+20);DEVOUT('Total:')
                setcor(1)
                DEVPOS(la-1,ci+13);DEVOUT(STRZERO(mcont_doc,4))
                DEVPOS(la-1,ci+27);DEVOUT(TRANSFORM(mtotal_doc,'99,999.99'))
                point := ACHOICE(li+3,ci+1,la-3,ca-1,m_cdmen,,,point)
                IF LASTKEY()=27
                        RESTSCREEN(00,00,24,79,tela)
                        EXIT
                ENDIF
        ELSE
                atencao('Nao existe nenhuma Parcela p/ o Aluno')
                EXIT
        ENDIF
ENDDO
******************************* FIM **************************************8

