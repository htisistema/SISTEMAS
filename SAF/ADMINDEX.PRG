FUNCTION admindex

setcor(1)
admdbf()
IF ! FILE(mcaminho+'visita.cdx')
        IF ! AbriArq('visita','vis','E');RETURN NIL;ENDIF
        INDEX ON codigo+DTOS(data) TAG cod TO (mcaminho+'visita')
        prog(23,50,50)
        INDEX ON DTOS(data)+codigo TAG dat TO (mcaminho+'visita')
        prog(23,50,100)
        vis->(DBCLOSEAREA())
ENDIF
IF ! FILE(mcaminho+'ativ.cdx')
        IF ! AbriArq('ativ','ativ','E');RETURN NIL;ENDIF
        INDEX ON codigo TAG cod TO (mcaminho+'ativ')
        prog(23,50,100)
        ativ->(DBCLOSEAREA())
ENDIF
IF ! FILE(mcaminho+'regiao.cdx')
        IF ! AbriArq('regiao','regi','E');RETURN NIL;ENDIF
        INDEX ON codigo TAG cod TO (mcaminho+'regiao')
        prog(23,50,100)
        regi->(DBCLOSEAREA())
ENDIF
IF ! FILE(mcaminho+'proposta.cdx')
        IF ! AbriArq('proposta','prop','E');RETURN NIL;ENDIF
        INDEX ON codigo+numprop TAG cod TO (mcaminho+'proposta')
        prog(23,50,50)
        INDEX ON numprop+codigo TAG cod TO (mcaminho+'proposta')
        prog(23,50,100)
        prop->(DBCLOSEAREA())
ENDIF
DBCOMMITALL()
RETURN NIL

