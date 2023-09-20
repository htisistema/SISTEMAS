****************************
* PREENCHIMENTO DE CHEQUES
****************************
MEMVAR getlist,mdata_sis

FUNCTION safpcheq
LOCAL mprg:='SAFPCHEQ',;
      lci,cci,lba,cba,lin,lin1,lin2,lin3,linhas := 0,linha:= 0,;
      opcao,tela1,telarec,mnome,mdata,mvalor,mmes,mextenso,i

lci := 05
cci := 08
lba := 20
cba := 74
opcao := SPACE(1)
telarec := SAVESCREEN(01,00,24,79)
WHILE .T.
        exibi_prg(mprg)
        mensagem('Preencha os campos  -  <ESC> p/Retornar')
        mnome := SPACE(35)
        mdata := mdata_sis
        mvalor := 0
        botao(lci,cci,lba,cba,,' PREENCHIMENTO DE CHEQUES ')
        @ lci+4,cci+1 TO lci+4,cba-1
        janela(lci+4,cba,' DEMONSTRATIVO DO CHEQUE ','*')
        SET INTEN ON
        DEVPOS(lci+1,cci+2);DEVOUT('Valor.........:')
        DEVPOS(lci+2,cci+2);DEVOUT('Nominal a ....:')
        DEVPOS(lci+3,cci+2);DEVOUT('Data do Cheque:')
        @ lci+1,cci+17 GET mvalor PICT '999,999,999.99'
        @ lci+2,cci+17 GET mnome
        @ lci+3,cci+17 GET mdata PICT '99/99/99'
        READ
        mmes := NMES(mdata)
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,telarec)
                RETURN NIL
        ENDIF
        mmes := NMES(mdata)
        setcor(3)
*       DEVPOS(lci+3,cci+24);DEVOUT(SUBSTR(DTOC(mdata),1,2)+' de '+RTRIM(mmes)+' de '+SUBSTR(DTOC(mdata),7))
        setcor(1)
        mextenso := '('+RTRIM(EXT(mvalor,53))+')'
        DEVPOS(lci+5,43);DEVOUT('Valor: ('+LTRIM(TRANSFORM(mvalor,'@E 999,999,999.99'))+')')

        linhas := MLCOUNT(mextenso,55)

        LIN := MEMOLINE(mextenso,55,1,,10)
*       DEVPOS(ROW()+1,cci+10);DEVOUT(IMPAC(LIN,.T.))
        DEVPOS(ROW()+1,cci+10);DEVOUT(LIN)
        linha := 1
        FOR linha = 2 TO linhas
                LIN := MEMOLINE(mextenso,55,linha,,10)
                DEVPOS(ROW()+1,cci+1);DEVOUT(LIN)
*               DEVPOS(ROW()+1,cci+1);DEVOUT(IMPAC(LIN,.T.))
        NEXT

        DEVPOS(ROW()+2,cci+1);DEVOUT('Nominal a '+mnome)
        DEVPOS(ROW()+2,cci+27);DEVOUT(sis_cidade+', '+SUBSTR(DTOC(mdata),1,2)+' de '+RTRIM(mmes)+' de '+SUBSTR(DTOC(mdata),7))
        mensagem('Confirma Impressao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'S'
                mextenso := '('+RTRIM(EXT(mvalor,53))+')'
                SET DEVI TO PRINT
                SET PRINT TO LPT1
                FOR i = 1 TO 1 //  mquantd
                        imprt('M','N')
                        DEVPOS(PROW(),53);DEVOUT('('+LTRIM(TRANSFORM(mvalor,'@E 999,999,999.99'))+')')

                        linhas := MLCOUNT(mextenso,55)

                        LIN := MEMOLINE(mextenso,55,1,,10)
*                       DEVPOS(PROW()+2,10);DEVOUT(IMPAC(LIN,.T.))
                        DEVPOS(PROW()+1,10);DEVOUT(RTRIM(LIN))
                        linha := 1

                        FOR linha = 2 TO linhas
                                LIN := MEMOLINE(mextenso,55,linha,,10)
                                DEVPOS(PROW()+1,02);DEVOUT(RTRIM(LIN))
*                               DEVPOS(PROW()+1,02);DEVOUT(IMPAC(LIN,.T.))
                        NEXT

                        DEVPOS(05,02);DEVOUT(mnome)
                        DEVPOS(PROW()+2,27);DEVOUT(RTRIM(sis_cidade))
                        DEVPOS(PROW(),42);DEVOUT(SUBSTR(DTOC(mdata),1,2))
                        DEVPOS(PROW(),48);DEVOUT(RTRIM(mmes))
                        DEVPOS(PROW(),63);DEVOUT(SUBSTR(DTOC(mdata),7))
                        DEVPOS(PROW()+11,00);DEVOUT(' ')
                        SETPRC(00,00)
                NEXT

                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSE
                LOOP
        ENDIF

ENDDO
RETURN NIL
******************************* F I M ******************************8

