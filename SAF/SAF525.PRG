****************************
* MENU DE EMISSAO DE CARNES
****************************

FUNCTION saf525
***************
LOCAL mprg := 'SAF525'
LOCAL mopcao,opcao,mopcao1,lci,cci,lba,cba,tela,tela1,mtit,mpag,mtot_icm,mtraco,;
      mquantd

lci = 10
cci = 10
lba = 16
cba = 45
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        exibi_prg(mprg)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Carnes em DEBITOS (Mensalidades) ' MESSAGE '** Emissao de Carnes de Aluno em DEBITO (Mensalidades)- <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Carnes do MES (Mensalidades)     ' MESSAGE '** Emissao de Carnes do MES (Mensalidades) - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' Carnes Avulso (Mensalidades)     ' MESSAGE '** Emissao de Carnes Avulsos (Mensalidades) - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' Carnes do MES (Escolinha)        ' MESSAGE '** Emissao de Carnes do MES (Escolinha) - <ESC> Retornar **'
        @ lci+5,cci+1 PROMPT ' Carnes do Avulsos                ' MESSAGE '** Emissao de Avulso - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *       EMISSAO DE CARNES EM DEBITOS
                         saf5251()
                CASE opcao = 2
                *       EMISSAO DE CARNES DO MES
                        saf5252()
                CASE opcao = 3
                *       EMISSAO DE CARNES AVULSOS
                        saf5253()
                CASE opcao = 4
                *       EMISSAO DE CARNES DO MES ESCOLINHA
                        saf5254()
                CASE opcao = 5
                *       EMISSAO DE CARNES AVULSOS
                        saf5255()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

