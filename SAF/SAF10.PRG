*******************************
* MENU DE ALUNOS
*******************************

FUNCTION saf10
**************
LOCAL tela,opcao,lci,cci,lba,cba

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 02
lba := 19
cba := 78

WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+1,cci+22);DEVOUT('Nome:')
        DEVPOS(lci+2,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+2,cci+13);DEVOUT('Sexo:')
        DEVPOS(lci+2,cci+21);DEVOUT('Nasc.:')
        DEVPOS(lci+2,cci+40);DEVOUT('Natural:')
        DEVPOS(lci+2,cci+65);DEVOUT('Estado:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nac.:')
        DEVPOS(lci+3,cci+17);DEVOUT('Ingresso:')
        DEVPOS(lci+3,cci+39);DEVOUT('Bolsa:')
        DEVPOS(lci+3,cci+48);DEVOUT('Transferido:')
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Nome do Pai:')
        DEVPOS(lci+5,cci+56);DEVOUT('Telefone:')
        DEVPOS(lci+6,cci+1);DEVOUT('Nome do Mae:')
        DEVPOS(lci+6,cci+56);DEVOUT('Telefone:')
        @ lci+7,cci+1 TO lci+7,cba-1
        DEVPOS(lci+8,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+8,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+9,cci+1);DEVOUT('Endereco...:')
        DEVPOS(lci+9,cci+51);DEVOUT('No.:')
        DEVPOS(lci+9,cci+63);DEVOUT('Apto.:')
        DEVPOS(lci+10,cci+1);DEVOUT('Bairro.....:')
        DEVPOS(lci+10,cci+36);DEVOUT('Cidade:')
        DEVPOS(lci+10,cci+66);DEVOUT('Estado:')
        DEVPOS(lci+11,cci+1);DEVOUT('CEP:')
        DEVPOS(lci+11,cci+16);DEVOUT('Fone:')
        DEVPOS(lci+12,cci+1);DEVOUT('Profissao.:')
        DEVPOS(lci+12,cci+35);DEVOUT('CPF.:')
        DEVPOS(lci+13,cci+1);DEVOUT('Fone......:')
        DEVPOS(lci+13,cci+24);DEVOUT('Fone:')
        DEVPOS(lci+13,cci+40);DEVOUT('Ramal:')
        limpa(23,00,23,79)
        opcao := 0
        DEVPOS(23,00);DEVOUT('Escolha a OPCAO:')
        setcor(1)
        @ 23,COL()+2 PROMPT ' INCLUSAO '
        @ 23,COL()+2 PROMPT ' VETERANOS '
        @ 23,COL()+2 PROMPT ' ALTERACAO '
        @ 23,COL()+2 PROMPT ' EXCLUSAO '
        @ 23,COL()+2 PROMPT ' RETORNA '
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
                        saf101()
                CASE opcao = 2
                        saf102()
                CASE opcao = 3
                        saf103()
                CASE opcao = 4
                        saf104()
                CASE opcao = 5
                        RESTSCREEN(00,00,24,79,tela)
                        RETURN NIL
        ENDCASE
ENDDO

