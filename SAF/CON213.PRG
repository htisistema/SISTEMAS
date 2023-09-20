*** CON213.PRG: Exclusao de Duplicatas (A PAGAR)
***
************************************************

FUNCTION con213
****************
LOCAL MPRG:='CON213'
LOCAL tela,mduplicata,lcod,last,mtipo,mfornec

IF ! ver_nivel(mprg,'EXCLUSAO DE DOCUMENTOS (C.APAGAR)','15')
        RETURN NIL
ENDIF

@ lci,cci+2 SAY " Contas a Pagar "
janela(lci,cba," Exclusao de Duplicatas ")
SAVE SCREE TO tela243
WHILE .T.
        REST SCREE FROM tela243
        ****************
        SELE('dupp');ORDSETFOCUS(1)
        ****************
        mtipo := SPACE(2)
        mduplicata=SPACE(10)
        mfornec = SPACE(4)
        mensagem('Digite o Numero da Duplicata. [ESC] Abandona.')
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,CT,PA,FR'
        @ lci+3,cci+27 GET mduplicata PICT '@!'
        READ
        IF LASTKEY()=27 .OR. mduplicata=SPACE(10) .OR. EMPTY(mtipo)
                RETURN
        ENDIF
        SEEK mtipo+mduplicata
        IF ! FOUND()
                atencao('Esta Duplicata nao existe! [ ды ] Continua.')
                LOOP
        ENDIF
        mcod_forn=VAL(fornec)
        @ lci+4,cci+27 GET mcod_forn PICT "9999" VALID v_fornece(mcod_forn,lci+4,cci+32) .AND. lim_get() WHEN men_get(lci+5,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        SEEK mtipo+mduplicata+' '+STRZERO(mcod_forn,4)
        IF ! FOUND() .OR. pago = 'B'
                atencao('Este Documento ja foi pago ou Excluido !!!')
                LOOP
        ELSEIF ! FOUND()
                atencao('Este Documento nao existe !!!')
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF

        setcor(1)
        @ lci+4,cci+27 SAY fornec PICT "9999"
        @ lci+4,COL() SAY '-'+cliente
        @ lci+5,cci+27 SAY DTOC(emissao)
        @ lci+6,cci+27 SAY DTOC(venc)
        @ lci+7,cci+27 SAY TRANSFORM(valor,"@E 99,999,999,999.99")
        @ lci+8,cci+27 SAY SUBSTR(banco,1,1)
        lcod=fornec
        last=valor
        Mensagem('Confirma Exclusao ? [s/N]:')
        INKEY(0)
        IF LASTKEY()=ASC("S") .OR. LASTKEY()=ASC("s")
                dupp->operador := cod_operado
                DELE
                DBUNLOCK()
                DBCOMMIT()

        ELSE
                DBUNLOCK()
                DBCOMMIT()
        ENDIF
ENDDO


