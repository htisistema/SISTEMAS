***************************************
* DESPROTEGER E VERIFICA O COMPUTADOR
***************************************
FUNCTION safdbf(mdir,mreindex)
************

LOCAL cor

LOCAL matriz :={}
limpa(24,00,24,79)
DEVPOS(24,00);DEVOUT('Aguarde um momento. Gerando Arquivos:')
limpa(24,39,24,79)
limpa(24,39,24,79)
DEVPOS(24,39);DEVOUT('SAFPRN =>')
progresso(24,50,0,24,63,00)
IF ! FILE('safprn.dbf')
        matriz:={ }
        AADD(matriz,{'impressora','C',20,0})
        AADD(matriz,{'tampag','N',3,0})
        AADD(matriz,{'a_comp17','C',20,0})
        AADD(matriz,{'d_comp17','C',20,0})
        AADD(matriz,{'a_comp20','C',20,0})
        AADD(matriz,{'d_comp20','C',20,0})
        AADD(matriz,{'a_elite','C',20,0})
        AADD(matriz,{'d_elite','C',20,0})
        AADD(matriz,{'a_negrito','C',20,0})
        AADD(matriz,{'d_negrito','C',20,0})
        AADD(matriz,{'a_expande','C',20,0})
        AADD(matriz,{'d_expande','C',20,0})
        AADD(matriz,{'a_8linpol','C',20,0})
        AADD(matriz,{'a_6linpol','C',20,0})
        AADD(matriz,{'a_italico','C',20,0})
        AADD(matriz,{'d_italico','C',20,0})
        DBCREATE('safprn.dbf',matriz)

        USE safprn ALIAS prin NEW EXCLUSIVE
        ADIREG()
        prin->impressora := 'HP LASER'
        prin->tampag := 59
        prin->a_comp17 :=   "CHR(27)+'(s16.67H'"
        prin->d_comp17 :=   "CHR(27)+'(s10H'"
        prin->a_comp20 :=   "CHR(27)+'(s16.67H'"
        prin->d_comp20 :=   "CHR(27)+'(s10H'"
        prin->a_elite :=    "CHR(27)+'(s12H'"
        prin->d_elite :=    "CHR(27)+'(s10H'"
        prin->a_negrito :=  "CHR(27)+'(s3B'"
        prin->d_negrito :=  "CHR(27)+'(s-3B'"
        prin->a_expande :=  "CHR(27)+'(s3B'"
        prin->d_expande :=  "CHR(27)+'(s-3B'"
        prin->a_8linpol := "CHR(27)+'(s1P'"
        prin->a_6linpol := "CHR(27)+'(s0P'"
        prin->a_italico := "4"
        prin->d_italico := "5"
        DBUNLOCK()
        ADIREG()
        prin->impressora := 'RIMA'
        prin->tampag := 59
        prin->a_comp17 :=   "CHR(15)"
        prin->d_comp17 :=   "CHR(18)"
        prin->a_comp20 :=   "CHR(27)+'['+CHR(5)"
        prin->d_comp20 :=   "CHR(27)+'['+CHR(0)"
        prin->a_elite :=    "CHR(27)+'['+CHR(2)"
        prin->d_elite :=    "CHR(27)+'['+CHR(1)"
        prin->a_negrito :=  "CHR(27)+'E'"
        prin->d_negrito :=  "CHR(27)+'F'"
        prin->a_expande :=  "CHR(27)+'W'+CHR(1)"
        prin->d_expande :=  "CHR(27)+'W'+CHR(0)"
        prin->a_8linpol := "CHR(27)+'0'"
        prin->a_6linpol := "CHR(27)+'2'"
        prin->a_italico := "4"
        prin->d_italico := "5"
        DBUNLOCK()
        ADIREG()
        prin->impressora := 'EPSON'
        prin->tampag := 59
        prin->a_comp17 :=   "CHR(15)"
        prin->d_comp17 :=   "CHR(18)"
        prin->a_comp20 :=   "CHR(27)+'M'+CHR(15)"
        prin->d_comp20 :=   "CHR(27)+'P'"
        prin->a_elite :=    "CHR(27)+'M'"
        prin->d_elite :=    "CHR(27)+'P'"
        prin->a_negrito :=  "CHR(27)+'E'"
        prin->d_negrito :=  "CHR(27)+'F'"
        prin->a_expande :=  "CHR(27)+'W'+CHR(1)"
        prin->d_expande :=  "CHR(27)+'W'+CHR(0)"
        prin->a_8linpol := "CHR(27)+'0'"
        prin->a_6linpol := "CHR(27)+'2'"
        prin->a_italico := "4"
        prin->d_italico := "5"
        DBUNLOCK()
        CLOSE prin
ENDIF
IF ! FILE('safprn.cdx')
        USE safprn ALIAS prin NEW SHARED
        INDEX ON impressora TAG imp TO safprn
        progresso(24,50,10,24,63,100)
        CLOSE prin
ENDIF
************************************************************************
IF ! FILE('caddisc.dbf')
        matriz:={ }
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'descr','C',20,0})
        AADD(matriz,{'tipo','C',1,0})
        AADD(matriz,{'descr_min','C',3,0})
        DBCREATE('caddisc.dbf',matriz)
ENDIF
IF ! FILE('caddisc.cdx')
        USE caddisc ALIAS cadd NEW SHARED
        INDEX ON cod_disc TAG cod TO caddisc
        progresso(24,50,10,24,63,100)
        CLOSE cadd
ENDIF
************************************************************************
IF ! FILE('cadnota.dbf')
        matriz:={ }
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'media_1','N',4,1})
        AADD(matriz,{'media_2','N',4,1})
        AADD(matriz,{'media_3','N',4,1})
        AADD(matriz,{'media_4','N',4,1})
        AADD(matriz,{'nt_recan','N',4,1})
        AADD(matriz,{'nt_prfi','N',4,1})
        AADD(matriz,{'falta_1','N',2,0})
        AADD(matriz,{'falta_2','N',2,0})
        AADD(matriz,{'falta_3','N',2,0})
        AADD(matriz,{'falta_4','N',2,0})
        AADD(matriz,{'ftpr_1','C',1,0})
        AADD(matriz,{'ftpr_2','C',1,0})
        AADD(matriz,{'ftpr_3','C',1,0})
        AADD(matriz,{'ftpr_4','C',1,0})
        AADD(matriz,{'ft_recan','C',1,0})
        AADD(matriz,{'ft_prfi','C',1,0})
        AADD(matriz,{'media_an','N',5,2})
        AADD(matriz,{'media_fi','N',5,2})
        AADD(matriz,{'situacao','C',2,0})
        AADD(matriz,{'media_un','N',4,1})
        AADD(matriz,{'media_fi2','N',5,2})
        DBCREATE('cadnota.dbf',matriz)
ENDIF
IF ! FILE('cadnota.cdx')
        USE cadnota ALIAS nota NEW SHARED
        INDEX ON matricula+cod_disc TAG mat TO cadnota
        INDEX ON gstt+cod_disc+matricula TAG gstt TO cadnota
        progresso(24,50,10,24,63,100)
        CLOSE nota
ENDIF
************************************************************************
IF ! FILE('cadaluno.cdx')
        USE cadaluno ALIAS aluno NEW SHARED
        INDEX ON responsa TAG resp TO  cadaluno
        INDEX ON gstt+aluno TAG gstt TO cadaluno
        INDEX ON aluno TAG alu TO  cadaluno
        INDEX ON matricula TAG mat TO cadaluno
        INDEX ON aluno->faixa_1+gstt+aluno TO cadaluno
        INDEX ON numero+gstt+aluno TAG num TO cadaluno
        progresso(24,50,10,24,63,100)
        CLOSE aluno
ENDIF
************************************************************************
IF ! FILE('cdmensal.dbf')
        matriz:={ }
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'parcela','C',2,0})
        AADD(matriz,{'data_pag','D',8,0})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'multa','N',10,2})
        AADD(matriz,{'compl','N',10,2})
        AADD(matriz,{'flag','C',1,0})
        DBCREATE('cadmensal.dbf',matriz)
ENDIF
IF ! FILE('cadmensal.cdx')
        USE cadmensal ALIAS cdmen NEW SHARED
        INDEX ON matricula+parcela TAG mat TO cadmensal
        progresso(24,50,10,24,63,100)
        CLOSE mensa
ENDIF
************************************************************************
IF ! FILE('faixa.dbf')
        matriz:={ }
        AADD(matriz,{'faixa','C',1,0})
        AADD(matriz,{'desconto','N',3,0})
        AADD(matriz,{'descricao','C',10,0})
        DBCREATE('faixa.dbf',matriz)
ENDIF
IF ! FILE('faixa.cdx')
        USE faixa.dbf ALIAS fax NEW SHARED
        ORDCREATE('faixa','faix','fax->faixa')
        progresso(24,50,10,24,63,100)
        CLOSE fax
ENDIF
************************************************************************
IF ! FILE('tabmensa.dbf')
        matriz:={ }
        AADD(matriz,{'gst','C',3,0})
        AADD(matriz,{'parcela','C',2,0})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'compl','N',10,2})
        AADD(matriz,{'data_venc','D',8,0})
        DBCREATE('tabmensa.dbf',matriz)
ENDIF
IF ! FILE('tabmensa.cdx')
        USE tabmensa ALIAS tabm NEW SHARED
        INDEX ON gst+parcela TAG gst TO tabmensa
        progresso(24,50,10,24,63,100)
        CLOSE tabm
ENDIF
************************************************************************
limpa(24,00,24,79)
atencao('Todos arquivos foram gerado com sucesso',.2)
RETURN NIL
