*** ADMSENHA.PRG: Senha
*************************

FUNCTION admsenha

SET COLOR TO +W/B
BOTAO(19, 64, 21, 76)
@ 19, 66 SAY "¶ Senha Ç"
@ 20, 66 SAY ""
senha="SABTAD"
for f=1 to 3
        pass=rread(10,3,"þ")
        pass=upper(pass)
        if pass=senha
                return
        endif
        ?? chr(7)
        @ 20,66 say space(10)
        @ 20,66 say ""
next f
set color to +w/n
clear
quadro("Acesso Nao Autorizado ao Sistema")
?? chr(7)
set color to w/n
@ 20,0 say " "
quit


