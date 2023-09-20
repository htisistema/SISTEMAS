MEMVAR nivel_acess
***********************
* MENU DE BOLETOS
***********************

FUNCTION safbolet(c,d,v)
***************
LOCAL mprg := 'safboleT'
LOCAL opcao,tela,tecla,lci,cci,lba,cba

IF ! ver_nivel(mprg,'EMISSAO DE BOLETOS BANCARIOS E PROMISSORIAS ','1245',nivel_acess)
        RETURN NIL
ENDIF
lci = 07
cci = 22
lba = 10
cba = 52
tela = SAVESCREEN(01,00,24,79)
opcao = 0
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba,,' MENU DE BOLETOS ')
        @ lci+1,cci+1 PROMPT ' 0-> Boleto BANDEPE         ' MESSAGE '** Emissao do Boleto BANDEPE - <ESC> p/Retornar **'
        @ lci+2,cci+1 PROMPT ' 1-> Boleto BANDEPE Avulso  ' MESSAGE '** Emissao do Boleto BANDEPE Avulso - <ESC> p/Retornar **'
/*
        @ lci+2,cci+1 PROMPT ' 1-> Boleto UNIBANCO        ' MESSAGE '** Emissao do Boleto UNIBANCO - <ESC> p/Retornar **'
        @ lci+3,cci+1 PROMPT ' 2-> Boleto BANCO REAL      ' MESSAGE '** Emissao do Boleto BANCO REAL - <ESC> p/Retornar **'
        @ lci+4,cci+1 PROMPT ' 3-> Boleto BANCO REAL 2    ' MESSAGE '** Emissao do Boleto BANCO REAL 2 - <ESC> p/Retornar **'
        @ lci+5,cci+1 PROMPT ' 4-> Boleto HSBC Bamerindus ' MESSAGE '** Emissao do Boleto HSBC Bamerindus - <ESC> p/Retornar **'
        @ lci+6,cci+1 PROMPT ' 5-> Boleto ITAU            ' MESSAGE '** Emissao do Boleto BANDEPE - <ESC> p/Retornar **'
        @ lci+7,cci+1 PROMPT ' 6-> Boleto BILBAO VIZCAYA  ' MESSAGE '** Emissao do Boleto BILBAO VIZCAIA - <ESC>p/Retornar **'
        @ lci+8,cci+1 PROMPT ' 7-> Boleto BRADESCO        ' MESSAGE '** Emissao do Boleto BRADESCO - <ESC>p/Retornar **'
        @ lci+9,cci+1 PROMPT ' 8-> Boleto BANCO BRASIL    ' MESSAGE '** Emissao do Boleto BANCO BRASIL - <ESC>p/Retornar **'
        @ lci+10,cci+1 PROMPT ' 9-> PROMISSORIAS           ' MESSAGE '** Emissao de PROMISSORIAS - <ESC>p/Retornar **'
        @ lci+11,cci+1 PROMPT ' A-> Dup.Pre-Impressa 1     ' MESSAGE '** Emissao de Duplicata Pre-Impressa 1 - <ESC>p/Retornar **'
        @ lci+12,cci+1 PROMPT ' B-> Dup.Pre-Impressa 2     ' MESSAGE '** Emissao de Duplicata Pre-Impressa 2 - <ESC>p/Retornar **'
        @ lci+13,cci+1 PROMPT ' C-> Boleto BANCO RURAL     ' MESSAGE '** Emissao do Boleto BANCO RURAL - <ESC>p/Retornar **'
*/
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *        BOLETO BANDEPE
                         safbole1('BANDEPE')
                CASE opcao = 2
                *        BOLETO BANDEPE AVULSO
                         safbolea('BANDEPE')
        ENDCASE
ENDDO
RETURN NIL

