*** CON243.PRG: Exclusao de Duplicatas (Receber)
***
************************************************

FUNCTION con243
****************
LOCAL MPRG:='CON243'
LOCAL tela,mduplicata,lcod,last,mtipo,mpoint,mnum_banco

PRIVATE mfornec

IF ! ver_nivel(mprg,'CONTA A RECEBER (EXCLUSAO DE DUPLICATAS)','1',nivel_acess)
        RETURN NIL
ENDIF

****************
SELE('dupr');ORDSETFOCUS(1)
****************
@ lci,cci+2 SAY " Contas a Receber "
janela(lci,cba," Exclusao de Documentos ")
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mtipo := SPACE(2)
        mduplicata=SPACE(10)
        mfornec = SPACE(4)
        mnum_banco := 0
        mensagem('Digite o Numero do Documento. [ESC] Abandona.')
        @ lci+2,cci+27 GET mtipo PICT '@!' VALID mtipo $ 'CH,DU,CT,FI,RE'
        @ lci+3,cci+27 GET mnum_banco PICT '999' VALID lim_get() WHEN mtipo <> 'DU' .AND. men_get(lci+4,cci+25,'Informe o No.do banco')
        @ lci+4,cci+27 GET mduplicata PICT '@!'
        READ
        IF LASTKEY()=27 .OR. mduplicata=SPACE(10) .OR. EMPTY(mtipo)
                RESTSCREEN(01,00,24,79,tela)
                RETURN
        ENDIF
        IF mnum_banco = 0
                SEEK mtipo+SPACE(3)+mduplicata
        ELSE
                SEEK mtipo+STRZERO(mnum_banco,3)+mduplicata
        ENDIF
        IF ! FOUND()
                atencao('Esta Documento nao existe! [ ды ] Continua.')
                LOOP
        ENDIF
        IF pago="B"
                atencao('Esta Documento foi Liquidada! [ ды ] Continua.')
                LOOP
        ENDIF

        setcor(1)
        mfornec := VAL(dupr->fornec)
        @ lci+5,cci+27 GET mfornec PICT "99999" VALID mcli(mfornec,lci+5,cci+27) .AND. lim_get() WHEN men_get(lci+6,cci+31,'Informe o Cod. do Cliente ou <F7>p/pesquisas de clientes')
        READ
        IF mnum_banco = 0
                SEEK mtipo+SPACE(3)+mduplicata+' '+STRZERO(mfornec,5)
        ELSE
                SEEK mtipo+STRZERO(mnum_banco,3)+mduplicata+' '+STRZERO(mfornec,5)
        ENDIF
        IF ! FOUND()
                atencao('Este Documentos nao existe em nossos arquivos')
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
        mpoint := RECNO()
        @ lci+6,cci+27 SAY DTOC(emissao)
        @ lci+7,cci+27 SAY DTOC(venc)
        @ lci+8,cci+27 SAY TRANSFORM(valor,"@E 99,999,999,999.99")
        @ lci+9,cci+27 SAY SUBSTR(banco,1,1)
        @ lci+10,cci+27 SAY vendedor
        ven(VAL(vendedor),lci+10,cci+31)
        @ lci+11,cci+27 SAY num_ped
        ****************
        SELE('dupr');ORDSETFOCUS(1)
        ****************
        GO mpoint
        lcod=fornec
        last=valor
        Mensagem('Confirma Exclusao ? [s/N]:')
        INKEY(0)
        IF LASTKEY()=ASC("S") .OR. LASTKEY()=ASC("s")
                dupr-> operador := cod_operado
                DELE
                COMMIT
                UNLOCK
                ******************
                SELE('dupr');ORDSETFOCUS(1)
                ******************
                UNLOCK
        ELSE
                COMMIT
                UNLOCK
        ENDIF
ENDDO


