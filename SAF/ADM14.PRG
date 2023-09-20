***
*** ADM14.PRG:  Menu de Ramo de Atividade
***

FUNCTION adm14

LOCAL xxx,cod,desc,tela := SAVESCREEN(00,00,24,79)

setcor(1)
BOTAO(9, 15, 19, 64,,' Atividade ')
@ 10, 16 TO 10,63
DEVPOS(10,16);DEVOUT(" C¢d   Descri‡„o                          ")
@ 11, 16 TO 11,63
IF ! AbriArq('ativ','ativ');RETURN NIL;ENDIF
ORDSETFOCUS(1)
GO TOP
pode=.t.
WHILE .t.
        mensagem("[I]nclus„o  [A]ltera‡„o  [E]xclus„o  [ ou ] Consulta   [ESC] Retorna")
        point=recno()
        FOR f=1 TO 7
                IF ! EOF()
                        @ 11+f,17 SAY codigo
                        @ 11+f,23 SAY atividade
                        SKIP
                ELSE
                        @ 11+f,17 SAY "  "
                        @ 11+f,23 SAY SPACE(40)
                ENDIF
        NEXT
        IF EOF()
                pode=.f.
        ENDIF
        go point
        s=INKEY(0)
        do CASE
                CASE s=asc("I") .or. s=asc("i")
                        WHILE .t.
                                BOTAO(19, 15, 21, 64)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                setcor(1)
                                Mensagem('Digite o C¢digo da Atividade a ser incluida. [ESC] Abandona')
                                cod=SPACE(2)
                                desc=SPACE(41)
                                @ 20,17 GET cod pict "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                seek cod
                                IF found()
                                        atencao('Esta Atividade j  existe!   Tecle [ ÄÙ ]')
                                        LOOP
                                ENDIF
                                @ 20,22 GET desc PICT '@!'
                                Mensagem('Digite a Descri‡„o (40 Caract. Maximo). [ESC] Abandona.')
                                READ
                                IF lastkey()=27
                                        go top
                                        EXIT
                                ENDIF
                                Mensagem('Confirma os dados digitados ? [S/n]:')
                                xxx:= INKEY(0)
                                IF xxx=asc("n") .or. xxx=asc("N")
                                        LOOP
                                ELSE
                                        ADIREG()
                                        ativ->codigo := cod
                                        ativ->atividade := desc
                                        DBCOMMIT()
                                        DBUNLOCK()
                                ENDIF
                                Mensagem('Incluir mais Atividades ? [S/n]:')
                                xxx:=INKEY(0)
                                IF xxx=asc("N") .or. xxx=asc("n")
                                        go top
                                        EXIT
                                ELSE
                                        LOOP
                                ENDIF
                        ENDDO
                CASE s=5
                        IF ! bof()
                                SKIP -1
                                pode=.t.
                        ENDIF
                CASE s=24
                        IF ! EOF() .and. pode=.t.
                                SKIP
                        ENDIF
                CASE s=asc("A") .or. s=asc("a")
                        WHILE .t.
                                BOTAO(19, 15, 21, 64)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                Mensagem('Digite o C¢digo da Atividade a ser modificada. [ESC] Abandona')
                                cod=SPACE(2)
                                @ 20,17 GET cod pict "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                seek cod
                                IF ! found()
                                        atencao('Esta Atividade n„o existe!   Tecle [ ÄÙ ]')
                                        LOOP
                                ENDIF
                                Mensagem('Modifique a Descri‡„o (40 Caract. Maximo). [ESC] Abandona.')
                                desc := atividade
                                @ 20,22 GET desc PICT '@!'
                                READ
                                Mensagem('Confirma os dados digitados ? [S/n]:')
                                xxx:= INKEY(0)
                                IF xxx=asc("n") .or. xxx=asc("N")
                                        LOOP
                                ELSE
                                        BLOQREG()
                                        ativ->atividade := desc
                                        DBCOMMIT()
                                        DBUNLOCK()
                                ENDIF
                                IF lastkey()=27
                                        go top
                                        EXIT
                                ENDIF
                                go point
                                FOR f=1 to 7
                                        IF ! EOF()
                                                @ 11+f,17 SAY codigo
                                                @ 11+f,23 SAY atividade
                                                SKIP
                                        ELSE
                                                @ 11+f,17 SAY "  "
                                                @ 11+f,23 SAY SPACE(40)
                                        ENDIF
                                NEXT f
                                Mensagem('Modificar mais Atividades ? [S/n]:')
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
                                BOTAO(19, 15, 21, 64)
                                @ 19, 20 SAY "Â"
                                @ 21, 20 SAY "Á"
                                @ 20, 20 SAY "³"
                                Mensagem('Digite o C¢digo da Atividade a ser Excluida. [ESC] Abandona')
                                cod=SPACE(2)
                                @ 20,17 GET cod pict "99"
                                READ
                                IF lastkey()=27 .or. cod="  "
                                        go top
                                        EXIT
                                ENDIF
                                seek cod
                                IF ! found()
                                        atencao('Esta Atividade n„o existe!   Tecle [ ÄÙ ]')
                                        LOOP
                                ENDIF
                                setcor(3)
                                @ 20,22 SAY atividade
                                setcor(1)
                                Mensagem('Confirma Exclus„o ? [s/N]:')
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
                                FOR f=1 to 7
                                        IF ! EOF()
                                                @ 11+f,17 SAY codigo
                                                @ 11+f,23 SAY atividade
                                                SKIP
                                        ELSE
                                                @ 11+f,17 SAY "   "
                                                @ 11+f,23 SAY SPACE(40)
                                        ENDIF
                                NEXT f
                                Mensagem('Excluir mais Atividades ? [S/n]:')
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
                        RETURN
        ENDCASE
ENDDO


