*******************
* MENU PRINCIPAL
*******************
FUNCTION saf0()
***************

LOCAL opcao

setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,00);DEVOUT(PADL(datseman(mdata_sis)+', '+imp_data(mdata_sis),80))
DEVPOS(00,01);DEVOUT(memp+resa)
limpa(23,00,23,79)
setcor(6)
DEVPOS(24,00);DEVOUT(PADL(mfi+rma+' >> SAFE '+mversao+' <<',80))
setcor(1)
CAIXA(mol,01,00,22,79,,,.F.)
SETCOLOR('b/w,x')
DEVPOS(22,00);DEVOUT('ЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫ')
SETCOLOR('n+/w')
*BANNER(04,'COLEGIO','n+/w,x')
*BANNER(11,'NOBREGA','n+/w,x')
BANNER(04,LEFT(memp,9),'n+/w,x')
BANNER(11,LEFT(resa,9),'n+/w,x')
*SUB_BANNER(17,35,tit)
SUB_BANNER(17,15,'GERAL - '+mdiretorio)
setcor(1)
DEVPOS(21,00);DEVOUT(PADC(sistema+' '+mversao,80))
DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
opcao:=0
WHILE .T.
        exibi_prg('SAF0')
        setcor(1)
        limpa(23,00,23,79)
        botao(01,01,03,12)
        botao(01,14,03,26)
        botao(01,28,03,36)
        botao(01,38,03,49)
        botao(01,51,03,63)
        botao(01,65,03,78)
        botao(19,72,21,78)
        @ 02,02 PROMPT ' Cadastro ' MESSAGE '** Cadastro, Altera‡ao e Exclusao **'
        @ 02,15 PROMPT ' Movimento ' MESSAGE '** Movimento Academico e Financeiro **'
        @ 02,29 PROMPT ' Caixa ' MESSAGE "** Recebimentos, Vales, Fechamento e TP's Pendentes **"
        @ 02,39 PROMPT ' Consulta ' MESSAGE '** Consultas do Academico e Financeiro **'
        @ 02,52 PROMPT ' Relatorio ' MESSAGE '** Relatorios em Geral **'
        @ 02,66 PROMPT ' Utilitario ' MESSAGE '** Utilitarios **'
        @ 20,73 PROMPT ' Fim ' MESSAGE '** Sai do Sistema **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                botao(19,72,21,78,'*')
                fim()
                LOOP
        ENDIF
        DO CASE
                CASE opcao = 1
        *            CADASTRO
                        saf1()
                CASE opcao = 2
        *            MOVIMENTO
                        saf2()
                CASE opcao = 3
        *            CAIXA
                        saf3()
                CASE opcao = 4
        *            CONSULTAS
                        setcor(1)
                        saf4()
                CASE opcao = 5
        *            RELATORIOS
                        setcor(1)
                        saf5()
                CASE opcao = 6
                        saf6()
                CASE opcao = 7
                        fim()
        ENDCASE
ENDDO
RETURN NIL


