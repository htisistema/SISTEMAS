*************************
* TIPO DE IMPRESSAO  ARQUIVOS OU IMPRESSORA
*************************

FUNCTION imp_arq(marq,mporta,op)
******************

LOCAL tela,lci,cci,lba,cba

lci := 05
cci := 03
lba := 13
cba := 78
IF op = NIL
        op := 'S'
ENDIF


tela := SAVESCREEN(01,00,24,79)
marq := '\'+CURDIR()+'\'+marq+SPACE(35-LEN('\'+CURDIR()+'\'+marq))

WHILE .T.
        mensagem('Escolha a configuracao ou <ESC> p/abandonar')
        IF mporta = 'R'
                mporta_imp := set->relatorio
        ELSEIF mporta = 'T'
                mporta_imp := set->terminal
        ELSEIF mporta = 'D'
                mporta_imp := set->dup_port
        ENDIF
        setcor(1)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Configuracao de IMPRESSAO ')
        botao(lci+1,cci+16,lci+3,cci+35)
        botao(lci+1,cci+40,lci+3,cci+59)
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Nome do Arquivo de impressao.........:')
        DEVPOS(lci+6,cci+1);DEVOUT('Tipo de Impressora [J]ato [M]atricial:')
        DEVPOS(lci+7,cci+1);DEVOUT('Porta Impressora.....................:')
        @ lci+2,cci+17 PROMPT '    IMPRESSORA    '
        @ lci+2,cci+41 PROMPT '     ARQUIVO      '        //+marq
        SET INTEN ON
        MENU TO mimp_tipo
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN .F.
        ENDIF
        IF mimp_tipo = 2
                botao(lci+1,cci+40,lci+3,cci+59,'*')
                mporta := 'PRN '
                @ lci+05,cci+40 GET marq PICT '@!' VALID IF(EMPTY(marq),.F.,.T.)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ELSE
                botao(lci+1,cci+16,lci+3,cci+35,'*')
        ENDIF
*       @ lci+6,cci+40 GET mtipo_imp PICT '@!' WHEN mimp_tipo = 1 VALID mtipo_imp $ 'M,J' .AND. IF(EMPTY(mtipo_imp),.F.,.T.)
        @ lci+6,cci+40 GET mtipo_imp PICT '@!' VALID mtipo_imp $ 'M,J' .AND. IF(EMPTY(mtipo_imp),.F.,.T.)
        @ lci+7,cci+40 GET mporta_imp PICT '@!' WHEN mimp_tipo = 1 VALID mporta_imp $ 'LPT1,LPT2,LPT3,LPT4,LPT5,COM1,COM2,COM3,COM4,PRN ' .AND. IF(EMPTY(mporta_imp),.F.,.T.)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF SUBSTR(mporta_imp,1,3) = 'LPT' .AND. mimp_tipo = 1
                IF ! PARALELA(VAL(SUBSTR(mporta_imp,4,1)))
                        atencao('Porta de impressao '+mporta_imp+' nao esta ativa')
                        LOOP
                ENDIF
                WHILE .T.
                        IF ! IMPOK(VAL(SUBSTR(mporta_imp,4,1)))
                                atencao('Porta de impressao '+mporta_imp+' nao esta pronta ou esta desligada - <ESC> p/abortar a impressao',4)
                                INKEY(.3)
                                IF LASTKEY() = 27
                                        EXIT
                                ENDIF
                                LOOP
                        ELSE
                                EXIT
                        ENDIF
                ENDDO
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        op := 'S'
        @ MAXROW()-1,COL()+1 GET op PICT '@!' VALID op $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF op = 'S'
                RESTSCREEN(01,00,24,79,tela)
                mensagem('Espere o final da impressao !!!')
                SET DEVI TO PRINT
                IF mimp_tipo = 1
                        SET PRINT TO (mporta_imp)
                ELSE
                        SET PRINT TO (marq)
                ENDIF
                RETURN .T.
        ELSE
                LOOP
        ENDIF
ENDDO
RETURN NIL
*************************** f i m *******************************************

