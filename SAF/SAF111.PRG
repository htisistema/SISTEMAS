*************************
* ALTERACAO DE MENSALIDADES
*************************
FUNCTION saf111
***************
LOCAL mprg:='SAF111'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,m_matriz:={},m_pos:={}
LOCAL mparcela,mvalor,mdata_venc,mano

IF ! ver_nivel(mprg,'ALTERACAO DE MENSALIDADES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 30
lba := 21
cba := 75

li := 13
ci := 02
lb := 16
cb := 37
tela := SAVESCREEN(01,00,24,79)
setcor(3)
CAIXA(mold,lci,cci,lba,cba,,,.T.)
setcor(1)

DEVPOS(lci,cci+2);DEVOUT(' Parcela: ')
setcor(3)
DEVPOS(lci+1,cci+1);DEVOUT('Parcela')
DEVPOS(lci+1,cci+10);DEVOUT('GSTT')
DEVPOS(lci+1,cci+16);DEVOUT('Valor R$')
DEVPOS(lci+1,cci+26);DEVOUT('Vencimento')
DEVPOS(lci+1,cci+36);DEVOUT('Operador')
setcor(1)
pode=.t.

*--------------------------------------------------
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
*--------------------------------------------------


WHILE .T.
        mano       := SPACE(2)
        mparcela   := SPACE(2)
        mdata_venc := SPACE(5)
        **************
        SELE('tabmen');ORDSETFOCUS(2)
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
                AADD(m_matriz,'  '+tabmen->parcela+'      '+tabmen->gst+'  '+TRANSFORM(tabmen->valor,'9,999.99')+'   '+tabmen->data_venc+'         '+tabmen->operador)
                AADD(m_pos,RECNO())
                SKIP
        ENDDO
        WHILE .T.
                **************
                SELE('tabmen');ORDSETFOCUS(2)
                **************
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
                point := ACHOICE(lci+2,cci+1,lba-1,cba-1,m_matriz,,,point)

                DO CASE
                        CASE LASTKEY()=27
                                EXIT
                        CASE LASTKEY() = 13
                                GO m_pos[point]
                                tela1      := SAVESCREEN(01,00,24,79)
                                mvalor     := tabmen->valor
                                mdata_venc := tabmen->data_venc
                                setcor(3)
                                CAIXA(mold,li,ci,lb,cb,,,.T.)
                                setcor(1)
                                janela(li,ci,' Alteracao Mensalid. ','*','*')
                                DEVPOS(li+1,ci+1);DEVOUT('Parcela')
                                DEVPOS(li+1,ci+10);DEVOUT(' GST')
                                DEVPOS(li+1,ci+16);DEVOUT('Valor R$')
                                DEVPOS(li+1,ci+26);DEVOUT('Dia Venc.')
                                setcor(3)
                                DEVPOS(li+2,ci+3);DEVOUT(tabmen->parcela)
                                DEVPOS(li+2,ci+11);DEVOUT(tabmen->gst)
                                setcor(1)
                                @ li+2,ci+16 GET mvalor PICT '9,999.99'
                                @ li+2,ci+26 GET mdata_venc PICT '99/99' VALID DDMM(mdata_venc)
                                READ
                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                mensagem('Confirma a ALTERACAO [S/n]:')
                                opcao := 'S'
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
                                m_matriz[point] :='  '+tabmen->parcela+'      '+tabmen->gst+'  '+TRANSFORM(mvalor,'9,999.99')+'   '+mdata_venc+'         '+cod_operado
*                               m_matriz[point] :='  '+tabmen->parcela+'      '+tabmen->gst+'  '+TRANSFORM(mvalor,'9,999.99')+'  '+cod_operado
                                tabmen->valor   := mvalor
                                tabmen->operador := cod_operado
                                tabmen->data_venc := mdata_venc
                                DBCOMMITALL()
                                DBUNLOCKALL()
                                RESTSCREEN(01,00,24,79,tela1)

                                *****************
                                SELE('cdmen');ORDSETFOCUS(6)
                                *****************
                                GO TOP
                                IF DBSEEK(tabmen->parcela+tabmen->gst)
                                        WHILE cdmen->parcela = tabmen->parcela .AND. ;
                                              SUBSTR(cdmen->gstt,1,2)+SUBSTR(cdmen->gstt,4) = tabmen->gst .AND. ! EOF()
                                                IF ! EMPTY(cdmen->data_pag) .OR. SUBSTR(DTOC(cdmen->data_venc),7,2) <> SUBSTR(mdiretorio,3,2)
                                                        SKIP
                                                        LOOP
                                                ENDIF

                                                BLOQREG()
                                                cdmen->vlr_parc   := tabmen->valor
*                                               mano  := SUBSTR(DTOC(cdmen->data_pag),7,2)
                                                cdmen->data_venc  := CTOD(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
                                                IF faixa_vlr(cdmen->bolsa) = 100
                                                        cdmen->desc_bolsa := tabmen->valor
                                                ELSE
                                                        cdmen->desc_bolsa := tabmen->valor * (faixa_vlr(cdmen->bolsa)/100)
                                                ENDIF
                                                DBUNLOCKALL()
                                                DBCOMMITALL()
                                                SKIP
                                        ENDDO
                                ENDIF
                ENDCASE
        ENDDO
ENDDO
RETURN NIL
*************************** f i m *******************************************

