FUNCTION jw

mcaminho := SPACE(13)
MSITUACAO := 'O'
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
IF ! AbriArq('sacmerc','merc');RETURN NIL;ENDIF
IF ! AbriArq('sacped_s','ped_s');RETURN NIL;ENDIF

SELE('ped_s');ORDSETFOCUS(4)
BLOQARQ()
WHILE ! EOF()
        DEVPOS(00,00);DEVOUT(pcod_merc+'-'+pmerc)
        ver_cod(VAL(pcod_merc))
        ped_s->pespecie := merc->especie
        SKIP
ENDDO
DBCOMMITALL()
DBUNLOCKALL()
QUIT
