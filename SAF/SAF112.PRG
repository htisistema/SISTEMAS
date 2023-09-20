*************************
* EXCLUSAO DE MENSALIDADES
*************************
FUNCTION saf112
***************
LOCAL mprg:='SAF112'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,m_matriz:={},m_pos:={}
LOCAL mparcela,mvalor

IF ! ver_nivel(mprg,'EXCLUSAO DE MENSALIDADES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 30
lba := 21
cba := 65

li := 13
ci := 02
lb := 16
cb := 27
tela := SAVESCREEN(01,00,24,79)
setcor(3)
CAIXA(mold,lci,cci,lba,cba,,,.T.)
setcor(1)

DEVPOS(lci,cci+2);DEVOUT(' Parcela: ')
setcor(3)
DEVPOS(lci+1,cci+1);DEVOUT('Parcela')
DEVPOS(lci+1,cci+10);DEVOUT('GSTT')
DEVPOS(lci+1,cci+16);DEVOUT('Valor R$')
DEVPOS(lci+1,cci+26);DEVOUT('Operador')
setcor(1)
pode=.t.
WHILE .T.
        mparcela := '  '
        **************
        SELE('tabmen');ORDSETFOCUS(3)
        ORDSETFOCUS(2)
        GO TOP
        ************
        @ lci,cci+12 GET mparcela PICT '99' VALID mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        tabmen->(DBSEEK(mparcela))
        ASIZE(m_matriz,0)
        ASIZE(m_pos,0)
        WHILE mparcela = tabmen->parcela
                AADD(m_matriz,'  '+tabmen->parcela+'      '+tabmen->gst+'  '+TRANSFORM(tabmen->valor,'9,999.99')+'  '+tabmen->operador)
                AADD(m_pos,RECNO())
                SKIP
        ENDDO
        WHILE .T.
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
                point := ACHOICE(lci+2,cci+1,lba-1,cba-1,m_matriz,,,point)

                DO CASE
                        CASE LASTKEY()=27
                                EXIT
                        CASE LASTKEY() = 13
                                GO m_pos[point]
                                tela1 := SAVESCREEN(01,00,24,79)
                                mvalor   := tabmen->valor
                                setcor(3)
                                CAIXA(mold,li,ci,lb,cb,,,.T.)
                                setcor(1)
                                janela(li,ci,' Alteracao Mensalid. ','*','*')
                                DEVPOS(li+1,ci+1);DEVOUT('Parcela')
                                DEVPOS(li+1,ci+10);DEVOUT(' GST')
                                DEVPOS(li+1,ci+16);DEVOUT('Valor R$')
                                setcor(3)
                                DEVPOS(li+2,ci+3);DEVOUT(tabmen->parcela)
                                DEVPOS(li+2,ci+11);DEVOUT(tabmen->gst)
                                DEVPOS(li+2,ci+16);DEVOUTPICT(tabmen->valor,'9,999.99')
                                setcor(1)
                                mensagem('Confirma a EXCLUSAO [S/n]:')
                                opcao := 'N'
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                IF ! BLOQREG()
                                        atencao('Nao foi possivel BLOQUEAR o registro no arquivo')
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                m_matriz[point] := '                            '
                                DELE
                                DBCOMMITALL()
                                DBUNLOCKALL()
                                RESTSCREEN(01,00,24,79,tela1)
                ENDCASE
        ENDDO



ENDDO
RETURN NIL
*************************** f i m *******************************************

