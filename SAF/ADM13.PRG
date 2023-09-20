**
*** ADM13.PRG:  Menu de Regi”es
***

FUNCTION adm13

LOCAL tela:=SAVESCREEN(00,00,24,79),cod,desc,cdvend


BOTAO(9, 15, 19, 64,,' Regioes ')
@ 10, 16 TO 10,63
DEVPOS(10,16);DEVOUT(" C¢d   Descri‡„o da Regi„o             V. ")
@ 11, 16 TO 11,63
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
ORDSETFOCUS(1)
go top
pode=.t.
WHILE .t.
        mensagem("[I]nclus„o  [A]ltera‡„o  [E]xclus„o  [ ou ] Consulta  [ESC] Retorna")
        point=recno()
        FOR f=1 TO 7
                IF ! EOF()
                        @ 11+f,17 SAY codigo
                        @ 11+f,23 SAY Regiao
                        @ 11+f,55 SAY codvend
                        SKIP
                ELSE
                        @ 11+f,17 SAY "  "
                        @ 11+f,23 SAY SPACE(30)
                        @ 11+f,55 SAY "  "
                ENDIF
        NEXT f
        IF eof()
                pode=.f.
        ENDIF
        go point
        s=INKEY(0)
        do CASE
                CASE s=asc("I") .or. s=asc("i")
                        WHILE .t.
                                BOTAO(19, 15, 21, 58)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                @ 19, 53 SAY "Â"
                                @ 21, 53 SAY "Á"
                                @ 20, 53 SAY "³"
                                Mensagem('Digite o C¢digo da Regiao a ser incluida. [ESC] Abandona')
                                cod=SPACE(2)
                                desc=SPACE(30)
                                cdvend=SPACE(3)
                                @ 20,17 GET cod PICT "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                SEEK cod
                                IF found()
                                        atencao("Esta Regi„o j  existe!   Tecle [ ÄÙ ]")
                                        LOOP
                                ENDIF
                                Mensagem('Digite a Descri‡„o, e o C¢digo do Vendedor Responsavel. [ESC] Fim')
                                @ 20,22 GET desc PICT '@!'
                                @ 20,55 GET cdvend PICT "999" VALID ver_ven(cdvend)
                                READ
                                IF lastkey()=27
                                        go top
                                        EXIT
                                ENDIF
                                Mensagem("Confirma os dados digitados ? [S/n]:")
                                xxx=INKEY(0)
                                IF xxx=asc("n") .or. xxx=asc("N") .OR. LASTKEY() = 27
                                        LOOP
                                ELSE
                                        ADIREG()
                                        reg->codigo := cod
                                        reg->Regiao := desc
                                        reg->codvend := cdvend
                                        DBCOMMIT()
                                        DBUNLOCK()
                                ENDIF
                                GO TOP
                        ENDDO
                CASE s=5
                        IF .not. bof()
                                SKIP -1
                                pode=.t.
                        ENDIF
                CASE s=24
                        IF ! EOF() .and. pode=.t.
                                SKIP
                        ENDIF
                CASE s=asc("A") .or. s=asc("a")
                        WHILE .t.
                                BOTAO(19, 15, 21, 58)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                @ 19, 53 SAY "Â"
                                @ 21, 53 SAY "Á"
                                @ 20, 53 SAY "³"
                                Mensagem("Digite o C¢digo da Regi„o a ser modificada. [ESC] Abandona")
                                cod=SPACE(2)
                                @ 20,17 GET cod PICT "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                SEEK cod
                                IF .not. found()
                                        atencao("Esta Regi„o n„o existe!   Tecle [ ÄÙ ]")
                                        LOOP
                                ENDIF
                                cod := reg->codigo
                                desc := reg->Regiao
                                cdvend := reg->codvend
                                @ 20,22 GET desc PICT '@!'
                                @ 20,55 GET cdvend PICT "999" VALID ver_ven(cdvend)
                                Mensagem("Modifique a Descri‡„o, e o Vendedor Respons vel. [ESC] Fim.")
                                READ
                                IF lastkey()=27
                                        go top
                                        EXIT
                                ENDIF
                                Mensagem("Confirma os dados digitados ? [S/n]:")
                                xxx=INKEY(0)
                                IF xxx=asc("n") .or. xxx=asc("N")
  * NAO
                                ELSE
                                        BLOQREG()
                                        reg->Regiao := desc
                                        reg->codvend := cdvend
                                ENDIF
                                go point
                                FOR f=1 TO 7
                                        IF ! EOF()
                                                @ 11+f,17 SAY codigo
                                                @ 11+f,23 SAY Regiao
                                                @ 11+f,55 SAY codvend
                                                SKIP
                                        ELSE
                                                @ 11+f,17 SAY "  "
                                                @ 11+f,23 SAY SPACE(40)
                                                @ 11+f,55 SAY "  "
                                        ENDIF
                                NEXT f
                                Mensagem("Modificar mais Regi”es ? [S/n]:")
                                xxx=INKEY(0)
                                IF xxx=asc("N") .or. xxx=asc("n")
                                        go top
                                        EXIT
                                ELSE
                                        LOOP
                                ENDIF
                        ENDDO
                CASE s=asc("E") .or. s=asc("e")
                        WHILE .t.
                                BOTAO(19, 15, 21, 58)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                @ 19, 53 SAY "Â"
                                @ 21, 53 SAY "Á"
                                @ 20, 53 SAY "³"
                                Mensagem("Digite o C¢digo da Regi„o a ser Excluida. [ESC] Abandona")
                                cod=SPACE(3)
                                @ 20,17 GET cod PICT "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                SEEK cod
                                IF .not. found()
                                        atencao("Esta Regi„o n„o existe!   Tecle [ ÄÙ ]")
                                        LOOP
                                ENDIF
                                @ 20,22 SAY Regiao
                                @ 20,55 SAY codvend
                                Mensagem("Confirma Exclus„o ? [s/N]:")
                                xx=INKEY(0)
                                IF xx=27
                                        go top
                                        EXIT
                                ENDIF
                                IF xx=asc("s") .or. xx=asc("S")
                                        BLOQREG()
                                        DELE
                                        PACK
                                ENDIF
                                go point
                                FOR f=1 TO 7
                                        IF ! EOF()
                                                @ 11+f,17 SAY codigo
                                                @ 11+f,23 SAY Regiao
                                                @ 11+f,55 SAY codvend
                                                SKIP
                                        ELSE
                                                @ 11+f,17 SAY "  "
                                                @ 11+f,23 SAY SPACE(30)
                                                @ 11+f,55 SAY "  "
                                        ENDIF
                                NEXT f
                                Mensagem("Excluir mais Regi”es ? [S/n]:")
                                xxx=INKEY(0)
                                IF xxx=asc("n") .or. xxx=asc("N")
                                        go top
                                        EXIT
                                ELSE
                                        LOOP
                                ENDIF
                        ENDDO
                CASE s=27
                        RESTSCREEN(00,00,24,79,tela)
                        CLOSE ALL
                        RETURN NIL
        endCASE
ENDDO


