**********************
* MENU DE CONFIGURACAO DO SISTEMA
**********************

FUNCTION saf61
***************

LOCAL opcao,lci,cci,lba,cba,tela,mdata_sis,mopcao,drverr,mvlr_multa:=0

lci = 10
cci = 37
lba = 13
cba = 64

li = 15
ci = 37
lb = 18
cb = 74

tela = SAVESCREEN(01,00,24,79)

opcao = 0
IF ! abriarq('safsetup','set');RETURN NIL;ENDIF

DO WHILE .T.
        exibi_prg('SAF61')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Nivel de Acesso          ' MESSAGE '** Configuracao do Nivel de Acesso, <ESC> p/Retorno **'
        @ lci+2,cci+1 PROMPT ' Valor Juros p/Dia        ' MESSAGE '** Valor do Juros p/Dia, <ESC> p/Retorno **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                        f3_nivel()
                CASE opcao = 2
                        mtela = SAVESCREEN(01,00,24,79)
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        mvlr_multa := 0
                        mvlr_juros := 0
                        mvlr_multa := set->vlr_multa
                        mvlr_juros := set->vlr_juros
                        DEVPOS(li+1,ci+1);DEVOUT('Valor Multa ao Mes (%)...:')
                        DEVPOS(li+2,ci+1);DEVOUT('Percentual de Juros p/Dia:')
                        @ li+1,ci+27 GET mvlr_multa PICT '999.99999'
                        @ li+2,ci+27 GET mvlr_juros PICT '999.99999'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,mtela)
                                LOOP
                        ENDIF
                        mopcao := 'S'
                        mensagem('Confirma o Valor Digitado [S/n]')
                        @ 23,COL()+1 GET mopcao VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(01,00,24,79,mtela)
                                LOOP
                        ENDIF
                        *****************
                        SELE('set')
                        *****************
                        IF LASTREC() = 0
                                ADIREG()
                                set->vlr_multa := mvlr_multa
                                set->vlr_juros := mvlr_juros
                                DBUNLOCKALL()
                                DBCOMMITALL()
                        ELSE
                                BLOQREG()
                                set->vlr_multa := mvlr_multa
                                set->vlr_juros := mvlr_juros
                                DBUNLOCKALL()
                                DBCOMMITALL()
                        ENDIF
                        RESTSCREEN(01,00,24,79,mtela)
        ENDCASE
ENDDO
RETURN NIL
**************************** F I M **********************************
