*************************
* EXCLUSAO DE CONTAS APAGAR
*************************

FUNCTION saf162
***************
LOCAL MPRG:='SAF162'
LOCAL tela,opcao,lci,cci,lba,cba

IF ! ver_nivel(mprg,'EXCLUSAO DE CONTAS APAGAR','15')
        RETURN NIL
ENDIF
lci := 05
cci := 00
lba := 23
cba := 79

tela:=SAVESCREEN(01,00,24,79)

WHILE .T.
        opcao := 'S'
        limpa(06,00,22,79)

        setcor(3)
        CAIXA(mold,06,00,09,44,,,.F.)
        setcor(1)
        janela(06,44,' Exclusao ')
        DEVPOS(06,02);DEVOUT(' Fornecedor e OUTROS ')
        DEVPOS(07,01);DEVOUT('Codigo____:')
        DEVPOS(07,19);DEVOUT('Data Cadastro_:')
        DEVPOS(08,01);DEVOUT('R.Social__:')

        setcor(3)
        CAIXA(mold,10,00,15,49,,,.F.)
        setcor(1)
        DEVPOS(10,02);DEVOUT(' Logradouro ')

        DEVPOS(11,01);DEVOUT('Endereco__:')
        DEVPOS(12,01);DEVOUT('Bairro____:')
        DEVPOS(13,01);DEVOUT('Cidade____:')
        DEVPOS(13,35);DEVOUT('UF___:')
        DEVPOS(14,01);DEVOUT('CEP_______:')
        DEVPOS(14,35);DEVOUT('C.P._:')

        setcor(3)
        CAIXA(mold,16,00,19,45,,,.F.)
        setcor(1)
        DEVPOS(16,02);DEVOUT(' Telefones ')

        DEVPOS(17,01);DEVOUT('Fone__:')
        DEVPOS(17,24);DEVOUT('Fone_:')
        DEVPOS(18,01);DEVOUT('Fax___:')

        setcor(3)
        CAIXA(mold,20,06,22,39,,,.F.)
        setcor(1)
        DEVPOS(20,08);DEVOUT(' Observacao ')

        setcor(3)
        CAIXA(mold,06,50,09,79,,,.F.)
        setcor(1)
        DEVPOS(06,52);DEVOUT(' Identidicacao ')
        DEVPOS(07,51);DEVOUT('CGC___:')
        DEVPOS(08,51);DEVOUT('Insc._:')

        setcor(3)
        CAIXA(mold,11,51,16,79,,,.F.)
        setcor(1)
        DEVPOS(11,53);DEVOUT(' Cobranca ')
        DEVPOS(12,52);DEVOUT('Banco_____:')
        DEVPOS(13,52);DEVOUT('Carteira__:')
        DEVPOS(14,52);DEVOUT('Prazo Pag.:')
        DEVPOS(15,52);DEVOUT('Limite____:')

        setcor(3)
        CAIXA(mold,17,48,22,79,,,.F.)
        setcor(1)
        DEVPOS(17,50);DEVOUT(' Contatos ')
        DEVPOS(18,49);DEVOUT('Gerente_____:')
        DEVPOS(19,49);DEVOUT('Faturamento_:')
        DEVPOS(20,49);DEVOUT('Cobranca____:')
        DEVPOS(21,49);DEVOUT('Vendedor____:')
        SET INTEN ON
        *****************
        SELE('ccpg');ORDSETFOCUS(1)
        *****************
        setcor(3)
        DEVPOS(07,13);DEVOUT(STRZERO(mcod_forn,4))
        DEVPOS(07,35);DEVOUT(data_cad)
        DEVPOS(08,13);DEVOUT(razao)

        DEVPOS(11,13);DEVOUT(endereco)
        DEVPOS(12,13);DEVOUT(bairro)
        DEVPOS(13,13);DEVOUT(cidade)
        DEVPOS(13,42);DEVOUT(uf)
        DEVPOS(14,13);DEVOUT(cep)
        DEVPOS(14,42);DEVOUT(cpostal)

        DEVPOS(17,09);DEVOUT(tel1)
        DEVPOS(17,31);DEVOUT(tel2)
        DEVPOS(18,09);DEVOUT(fax)

        DEVPOS(07,59);DEVOUTPICT(cgc,'99.999.999/9999-99')
        DEVPOS(08,59);DEVOUT(insc)

        DEVPOS(12,63);DEVOUT(banco)
        DEVPOS(13,63);DEVOUT(carteira)
        DEVPOS(14,63);DEVOUTPICT(prazo_pag,'@R 99-99-99')
        DEVPOS(15,63);DEVOUTPICT(limite,'9,999,999.99')

        DEVPOS(18,62);DEVOUT(ct_gerente)
        DEVPOS(19,62);DEVOUT(ct_fatura)
        DEVPOS(20,62);DEVOUT(ct_cobran)
        DEVPOS(21,62);DEVOUT(ct_vendedo)

        DEVPOS(21,08);DEVOUT(obs)
        setcor(1)
        mensagem('Confirma Exclusao do Fabricante [S/n]:')
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ELSE
                IF ! BLOQREG()
                        atencao('Nao foi possivel bloquear o registro')
                        LOOP
                ENDIF
                DELE
                DBCOMMIT()
                DBUNLOCK()
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
ENDDO
RETURN NIL
*********************** f i m ************************************

