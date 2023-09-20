***************************************
* DESPROTEGER E VERIFICA O COMPUTADOR
***************************************
FUNCTION safdbf(mdir,mreindex)
************

LOCAL cor

LOCAL matriz :={}
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
limpa(24,00,24,79)
DEVPOS(24,00);DEVOUT('Aguarde um momento. Gerando Arquivos:')
limpa(24,39,24,79)
DEVPOS(24,39);DEVOUT('SAFPRN =>')
prog(24,51,10)
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safprn.dbf')
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
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safprn.dbf',matriz)

        IF ! abriarq('safprn','prin','E');RETURN NIL;ENDIF
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
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safprn.cdx')
        IF ! abriarq('safprn','prin');RETURN NIL;ENDIF
        INDEX ON impressora TAG imp TO ('\'+mdrive+'\'+mdiretorio+'\safprn')
        prog(24,51,100)
        CLOSE prin
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('CADDISC =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\caddisc.dbf')
        matriz:={ }
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'descr','C',20,0})
        AADD(matriz,{'tipo','C',1,0})
        AADD(matriz,{'descr_min','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\caddisc.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\caddisc.cdx')
        IF ! abriarq('caddisc','cadd');RETURN NIL;ENDIF
        INDEX ON cod_disc TAG cod TO ('\'+mdrive+'\'+mdiretorio+'\caddisc')
        prog(24,51,100)
        CLOSE cadd
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('CADNOTA =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadnota.dbf')
        matriz:={ }
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'numero','C',3,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'media_1','N',4,1})
        AADD(matriz,{'conce_1','C',1,0})
        AADD(matriz,{'opera_1','C',3,0})
        AADD(matriz,{'media_2','N',4,1})
        AADD(matriz,{'conce_2','C',1,0})
        AADD(matriz,{'opera_2','C',3,0})
        AADD(matriz,{'media_3','N',4,1})
        AADD(matriz,{'conce_3','C',1,0})
        AADD(matriz,{'opera_3','C',3,0})
        AADD(matriz,{'media_4','N',4,1})
        AADD(matriz,{'conce_4','C',1,0})
        AADD(matriz,{'opera_4','C',3,0})
        AADD(matriz,{'conc_md','C',1,0})
        AADD(matriz,{'oper_md','C',3,0})
        AADD(matriz,{'nt_recan','N',4,1})
        AADD(matriz,{'conce_rc','C',1,0})
        AADD(matriz,{'opera_ra','C',3,0})
        AADD(matriz,{'conc_mr','C',1,0})
        AADD(matriz,{'oper_mr','C',3,0})
        AADD(matriz,{'nt_prfi','N',4,1})
        AADD(matriz,{'conce_pf','C',1,0})
        AADD(matriz,{'opera_pf','C',3,0})
        AADD(matriz,{'conc_mf','C',1,0})
        AADD(matriz,{'oper_mf','C',3,0})
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
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\cadnota.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadnota.cdx')
        IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
        INDEX ON matricula+cod_disc TAG mat TO ('\'+mdrive+'\'+mdiretorio+'\cadnota')
        prog(24,51,30)
        INDEX ON gstt+cod_disc+matricula TAG gstt TO ('\'+mdrive+'\'+mdiretorio+'\cadnota')
        prog(24,51,70)
        INDEX ON gstt+cod_disc+numero TAG num TO ('\'+mdrive+'\'+mdiretorio+'\cadnota')
        prog(24,51,100)
        CLOSE nota
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('CADALUNO=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadaluno.dbf')
        matcad:={ }
        AADD(matcad,{'contrato','C',8,0})
        AADD(matcad,{'numero','C',3,0})
        AADD(matcad,{'gstt','C',4,0})
        AADD(matcad,{'aluno','C',40,0})
        AADD(matcad,{'aluno1','C',55,0})
        AADD(matcad,{'matricula','C',6,0})
        AADD(matcad,{'sexo','C',1,0})
        AADD(matcad,{'a_datan','D',8,0})
        AADD(matcad,{'a_natur','C',14,0})
        AADD(matcad,{'a_estado','C',2,0})
        AADD(matcad,{'a_nacion','C',8,0})
        AADD(matcad,{'data_ing','D',8,0})
        AADD(matcad,{'ing_esco','D',8,0})
        AADD(matcad,{'pai','C',40,0})
        AADD(matcad,{'p_fone','C',8,0})
        AADD(matcad,{'mae','C',40,0})
        AADD(matcad,{'m_fone','C',8,0})
        AADD(matcad,{'responsa','C',40,0})
        AADD(matcad,{'r_rua','C',35,0})
        AADD(matcad,{'r_num','C',5,0})
        AADD(matcad,{'r_apto','C',5,0})
        AADD(matcad,{'r_bairro','C',20,0})
        AADD(matcad,{'r_cidade','C',20,0})
        AADD(matcad,{'r_estado','C',2,0})
        AADD(matcad,{'r_cep','C',8,0})
        AADD(matcad,{'r_fone','C',8,0})
        AADD(matcad,{'r_prof','C',20,0})
        AADD(matcad,{'r_cpf','C',12,0})
        AADD(matcad,{'r_fone1','C',8,0})
        AADD(matcad,{'r_fone2','C',8,0})
        AADD(matcad,{'r_ramal','C',4,0})
        AADD(matcad,{'r_datnasc','D',8,0})
        AADD(matcad,{'area','C',1,0})
        AADD(matcad,{'disciplina','C',3,0})
        AADD(matcad,{'bloqueio','C',1,0})
        AADD(matcad,{'bloqueio2','C',1,0})
        AADD(matcad,{'bloqueio3','C',1,0})
        AADD(matcad,{'bloqueio4','C',1,0})
        AADD(matcad,{'dt_trans', 'D',8,0})
        AADD(matcad,{'faixa_1','C',1,0})
        AADD(matcad,{'faixa_2','C',1,0})
        AADD(matcad,{'faixa_3','C',1,0})
        AADD(matcad,{'faixa_4','C',1,0})
        AADD(matcad,{'faixa_5','C',1,0})
        AADD(matcad,{'faixa_6','C',1,0})
        AADD(matcad,{'faixa_7','C',1,0})
        AADD(matcad,{'faixa_8','C',1,0})
        AADD(matcad,{'faixa_9','C',1,0})
        AADD(matcad,{'faixa_10','C',1,0})
        AADD(matcad,{'faixa_11','C',1,0})
        AADD(matcad,{'faixa_12','C',1,0})
        AADD(matcad,{'dt_m_fai','D',8,0})
        AADD(matcad,{'dt_tranf','D',8,0})
        AADD(matcad,{'op_tranf','C',3,0})
        AADD(matcad,{'dt_exclu','D',8,0})
        AADD(matcad,{'op_exclu','C',3,0})
        AADD(matcad,{'flag','C',1,0})
        AADD(matcad,{'obs','C',50,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\cadaluno.dbf',matcad)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadaluno.cdx')
        IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
        INDEX ON responsa TAG resp TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,10)
        INDEX ON gstt+aluno TAG gstt TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,20)
        INDEX ON aluno TAG alu TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,30)
        INDEX ON matricula TAG mat TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,40)
        INDEX ON faixa_1+gstt+aluno TAG fx TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,50)
        INDEX ON gstt+numero+aluno TAG num TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,60)
        INDEX ON gstt+SUBSTR(DTOC(a_datan),4,2)+SUBSTR(DTOC(a_datan),1,2)+aluno TAG gsttnasc TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,70)
        INDEX ON SUBSTR(DTOC(a_datan),4,2)+SUBSTR(DTOC(a_datan),1,2)+gstt+aluno TAG nasc TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,80)
        INDEX ON contrato+aluno TAG cont TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,90)
        INDEX ON r_bairro+r_cidade+aluno TAG bairr TO ('\'+mdrive+'\'+mdiretorio+'\cadaluno')
        prog(24,51,100)
        CLOSE aluno
ENDIF
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('SAFNOMAT=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safnomat.dbf')
        matcad:={ }
        AADD(matcad,{'no_mat','C',6,0})
        AADD(matcad,{'no_data','D',8,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safnomat.dbf',matcad)
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('CADMENSAL=>')
*IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cdmensal.dbf')
IF ! FILE('\'+mdrive+'\cdmensal.dbf')
        matriz:={ }
        AADD(matriz,{'ano','C',4,0})
        AADD(matriz,{'contrato','C',8,0})
        AADD(matriz,{'tipo','C',2,0})
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'aluno','C',40,0})
        AADD(matriz,{'bolsa','C',1,0})
        AADD(matriz,{'parcela','C',2,0})
        AADD(matriz,{'data_pro','D',8,0})
        AADD(matriz,{'data_venc','D',8,0})
        AADD(matriz,{'data_pag','D',8,0})
        AADD(matriz,{'vlr_parc','N',10,2})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'multa','N',10,2})
        AADD(matriz,{'compl','N',10,2})
        AADD(matriz,{'desc_bolsa','N',10,2})
        AADD(matriz,{'flag','C',1,0})
        AADD(matriz,{'operador','C',3,0})
        AADD(matriz,{'conta','C',1,0})
        AADD(matriz,{'nosso_no','C',7,0})
        AADD(matriz,{'nosso_dv','C',1,0})
*       DBCREATE('\'+mdrive+'\'+mdiretorio+'\cdmensal.dbf',matriz)
        DBCREATE('\'+mdrive+'\cdmensal.dbf',matriz)
ENDIF
*IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cdmensal.cdx')
IF ! FILE('\'+mdrive+'\cdmensal.cdx')
        IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
        INDEX ON matricula+ano+parcela+tipo TAG mat TO ('\'+mdrive+'\cdmensal')
        prog(24,51,10)
        INDEX ON parcela+gstt+bolsa+ano TAG parc TO ('\'+mdrive+'\cdmensal')
        prog(24,51,20)
        INDEX ON DTOS(data_pag)+gstt+bolsa+ano TAG dat_pag TO ('\'+mdrive+'\cdmensal')
        prog(24,51,30)
        INDEX ON DTOS(data_pag)+parcela+gstt+ano TAG dat_pac TO ('\'+mdrive+'\cdmensal')
        prog(24,51,40)
        INDEX ON parcela+matricula TAG par_mat TO ('\'+mdrive+'\cdmensal')
        prog(24,51,50)
        INDEX ON parcela+SUBSTR(gstt,1,2)+SUBSTR(gstt,4)+ano TAG parc_gst TO ('\'+mdrive+'\cdmensal')
        prog(24,51,60)
        INDEX ON matricula+DTOS(data_pag)+ano TAG mat_pag TO ('\'+mdrive+'\cdmensal')
        prog(24,51,70)
        INDEX ON matricula+DTOS(data_venc)+ano TAG mat_venc TO ('\'+mdrive+'\cdmensal')
        prog(24,51,80)
        INDEX ON parcela+contrato+ano TAG parc_cont TO ('\'+mdrive+'\cdmensal')
        prog(24,51,90)
        INDEX ON DTOS(data_pag)+matricula+ano TAG dat_mat TO ('\'+mdrive+'\cdmensal')
        prog(24,51,100)
        INDEX ON ano+gstt+matricula TAG ano_gst TO ('\'+mdrive+'\cdmensal')
        prog(24,51,110)
        CLOSE cdmen
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('FAIXA    =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\faixa.dbf')
        matriz:={ }
        AADD(matriz,{'faixa','C',1,0})
        AADD(matriz,{'desconto','N',3,0})
        AADD(matriz,{'descricao','C',10,0})
        AADD(matriz,{'operador','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\faixa.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\faixa.cdx')
        IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
        INDEX ON fx->faixa TAG faix TO ('\'+mdrive+'\'+mdiretorio+'\faixa')
        prog(24,51,100)
        CLOSE fx
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('TABMENSA =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\tabmensa.dbf')
        matriz:={ }
        AADD(matriz,{'gst','C',3,0})
        AADD(matriz,{'parcela','C',2,0})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'compl','N',10,2})
        AADD(matriz,{'data_venc','C',5,0})
        AADD(matriz,{'operador','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\tabmensa.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\tabmensa.cdx')
        IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
        INDEX ON gst+parcela TAG gst TO ('\'+mdrive+'\'+mdiretorio+'\tabmensa')
        prog(24,51,30)
        INDEX ON parcela+gst TAG parc TO ('\'+mdrive+'\'+mdiretorio+'\tabmensa')
        prog(24,51,70)
        INDEX ON gst+parcela TAG gst TO ('\'+mdrive+'\'+mdiretorio+'\tabmensa')
        prog(24,51,100)
        CLOSE tabmen
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('TABGSTT  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\tabgstt.dbf')
        matriz:={ }
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'disci1','C',10,0})
        AADD(matriz,{'disci2','C',10,0})
        AADD(matriz,{'ch','N',6,0})
        AADD(matriz,{'operador','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\tabgstt.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\tabgstt.cdx')
        IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
        INDEX ON gstt+cod_disc TAG gstt TO ('\'+mdrive+'\'+mdiretorio+'\tabgstt')
        prog(24,51,100)
        CLOSE tbgstt
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('CADPROF  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadprof.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',3,0})
        AADD(matriz,{'nome','C',30,0})
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'cod_disc','C',2,0})
        AADD(matriz,{'disciplina','C',20,0})
        AADD(matriz,{'operador','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\cadprof.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cadprof.cdx')
        IF ! abriarq('cadprof','prof');RETURN NIL;ENDIF
        INDEX ON codigo TAG cod TO ('\'+mdrive+'\'+mdiretorio+'\cadprof')
        prog(24,51,50)
        INDEX ON cod_disc+gstt TAG gst TO ('\'+mdrive+'\'+mdiretorio+'\cadprof')
        prog(24,51,100)
        CLOSE prof
ENDIF
************************************************************************
limpa(24,39,24,79)
prog(24,51,10)
DEVPOS(24,39);DEVOUT('REMESSA  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\remessa.dbf')
        matriz:={ }
        AADD(matriz,{'linha1','C',60,0})
        AADD(matriz,{'linha2','C',60,0})
        AADD(matriz,{'linha3','C',60,0})
        AADD(matriz,{'linha4','C',60,0})
        AADD(matriz,{'linha5','C',60,0})
        AADD(matriz,{'linha6','C',60,0})
        AADD(matriz,{'linha7','C',60,0})
        AADD(matriz,{'linha8','C',60,0})
        AADD(matriz,{'linha9','C',60,0})
        AADD(matriz,{'linha10','C',60,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\remessa.dbf',matriz)
ENDIF
************************************************************************
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safsetup.dbf')
        matriz:={ }
        AADD(matriz,{'porta','C',4,0})
        AADD(matriz,{'vlr_multa','N',15,5})
        AADD(matriz,{'vlr_juros','N',15,5})
        AADD(matriz,{'sysdatlm','C',12,0})
        DBCREATE('safsetup.dbf',matriz)
ENDIF
************************************************************************
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safcontr.dbf')
        matriz:={ }
        AADD(matriz,{'nome','C',40,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'grau','C',1,0})
        AADD(matriz,{'serie','C',1,0})
        AADD(matriz,{'turma','C',1,0})
        AADD(matriz,{'turno','C',1,0})
        AADD(matriz,{'responsa','C',40,0})
        AADD(matriz,{'profissao','C',20,0})
        AADD(matriz,{'cpf','C',12,0})
        AADD(matriz,{'rua','C',35,0})
        AADD(matriz,{'no','C',5,0})
        AADD(matriz,{'compl','C',5,0})
        AADD(matriz,{'bairro','C',20,0})
        AADD(matriz,{'cidade','C',20,0})
        AADD(matriz,{'estado','C',2,0})
        AADD(matriz,{'cep','C',8,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safcontr.dbf',matriz)
ENDIF
************************************************************************
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safpromi.dbf')
        matriz:={ }
        AADD(matriz,{'nome','C',40,0})
        AADD(matriz,{'matricula','C',6,0})
        AADD(matriz,{'grau','C',1,0})
        AADD(matriz,{'serie','C',1,0})
        AADD(matriz,{'turma','C',1,0})
        AADD(matriz,{'turno','C',1,0})
        AADD(matriz,{'responsa','C',40,0})
        AADD(matriz,{'profissao','C',20,0})
        AADD(matriz,{'cpf','C',12,0})
        AADD(matriz,{'rua','C',35,0})
        AADD(matriz,{'no','C',5,0})
        AADD(matriz,{'compl','C',5,0})
        AADD(matriz,{'bairro','C',20,0})
        AADD(matriz,{'cidade','C',20,0})
        AADD(matriz,{'estado','C',2,0})
        AADD(matriz,{'cep','C',8,0})
        AADD(matriz,{'vlr_promi','N',12,2})
        AADD(matriz,{'vlr_matri','N',12,2})
        AADD(matriz,{'extenso','C',12,2})
        AADD(matriz,{'extenso1','C',12,2})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safpromi.dbf',matriz)
ENDIF
************************************************************************
DEVPOS(24,39);DEVOUT('SAFCCPG  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safccpg.dbf')
        matriz:={ }
        AADD(matriz,{'cod_forn','C',4,0})
        AADD(matriz,{'razao','C',40,0})
        AADD(matriz,{'tipo','C',2,0})
        AADD(matriz,{'data_cad','D',8,0})
        AADD(matriz,{'ult_comp','D',8,0})
        AADD(matriz,{'endereco','C',35,0})
        AADD(matriz,{'bairro','C',20,0})
        AADD(matriz,{'cidade','C',20,0})
        AADD(matriz,{'uf','C',2,0})
        AADD(matriz,{'cep','C',9,0})
        AADD(matriz,{'cpostal','C',6,0})
        AADD(matriz,{'email','C',40,0})
        AADD(matriz,{'tel1','C',14,0})
        AADD(matriz,{'tel2','C',14,0})
        AADD(matriz,{'fax','C',14,0})
        AADD(matriz,{'cgc','C',14,0})
        AADD(matriz,{'insc','C',14,0})
        AADD(matriz,{'cpf','C',11,0})
        AADD(matriz,{'carteira','C',1,0})
        AADD(matriz,{'banco','C',1,0})
        AADD(matriz,{'prazo_pag','C',8,0})
        AADD(matriz,{'ct_gerente','C',15,0})
        AADD(matriz,{'ct_vendedo','C',15,0})
        AADD(matriz,{'ct_fatura','C',15,0})
        AADD(matriz,{'ct_cobran','C',15,0})
        AADD(matriz,{'limite','N',10,2})
        AADD(matriz,{'obs','C',30,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safccpg.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safccpg.cdx')
        IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
        ORDCREATE('\'+mdrive+'\'+mdiretorio+'\safccpg','codigo','ccpg->cod_forn')
        prog(24,50,30)
        ORDCREATE('\'+mdrive+'\'+mdiretorio+'\safccpg','nome','ccpg->razao')
        prog(24,50,40)
        ORDCREATE('\'+mdrive+'\'+mdiretorio+'\safccpg','cgc','ccpg->cgc')
        prog(24,50,100)
        INKEY(.3)
        CLOSE ccpg
ENDIF
**********************************************************************************************
DEVPOS(24,39);DEVOUT('SACDUPP  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\sacdupp.dbf')
        matriz:={ }
        AADD(matriz,{'tipo','C',2,0})
        AADD(matriz,{'numero','C',3,0})
        AADD(matriz,{'duplicata','C',10,0})
        AADD(matriz,{'valor_dup','N',10,2})
        AADD(matriz,{'tip_for','C',2,0})
        AADD(matriz,{'fornec','C',4,0})
        AADD(matriz,{'cliente','C',40,0})
        AADD(matriz,{'emissao','D',8,0})
        AADD(matriz,{'venc','D',8,0})
        AADD(matriz,{'datpag','D',8,0})
        AADD(matriz,{'banco','C',3,0})
        AADD(matriz,{'agencia','C',8,0})
        AADD(matriz,{'c_c','C',13,0})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'vlpago','N',10,2})
        AADD(matriz,{'desc','N',10,2})
        AADD(matriz,{'juros','N',10,2})
        AADD(matriz,{'pago','C',1,0})
        AADD(matriz,{'conta','C',1,0})
        AADD(matriz,{'obs','C',60,0})
        AADD(matriz,{'nbanco','C',4,0})
        AADD(matriz,{'cheque','C',8,0})
        AADD(matriz,{'vendedor','C',3,0})
        AADD(matriz,{'num_ped','C',6,0})
        AADD(matriz,{'operador','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\sacdupp.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\sacdupp.cdx')       // DUPPAG1
        IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
        INDEX ON tipo+duplicata+pago+fornec+DTOS(venc) TAG dup TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,20)
        INDEX ON fornec+pago+duplicata TAG fornec TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,40)
        INDEX ON DTOS(datpag)+tipo+duplicata+fornec TAG datapg TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,60)
        INDEX ON DTOS(venc)+fornec+banco+duplicata TAG venc TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,70)
        INDEX ON DTOS(emissao) TAG emissao TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,80)
        INDEX ON cliente+fornec+pago+DTOS(venc) TAG cli TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,90)
        INDEX ON num_ped+fornec TAG ped TO ('\'+mdrive+'\'+mdiretorio+'\sacdupp')
        prog(24,50,100)
        INKEY(0.3)
        CLOSE dupp
ENDIF
**********************************************************************************************
limpa(24,39,24,79)
DEVPOS(24,39);DEVOUT('SACCONF=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\sacconf.dbf')
        matriz:={ }
        AADD(matriz,{'modulo','C',20,0})
        AADD(matriz,{'descri','C',40,0})
        AADD(matriz,{'nivel','C',9,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\sacconf.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\sacconf.cdx')
        IF ! abriarq('sacconf','conf');RETURN NIL;ENDIF
        INDEX ON conf->descri TAG desc TO ('\'+mdrive+'\'+mdiretorio+'\sacconf')
        INDEX ON conf->modulo TAG modu TO ('\'+mdrive+'\'+mdiretorio+'\sacconf')
        CLOSE conf
ENDIF
**********************************************************************************************
DEVPOS(24,39);DEVOUT('DISCALU=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\discalu.dbf')
        matriz:={ }
        AADD(matriz,{'responsa','C',40,0})
        AADD(matriz,{'aluno','C',50,0})
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'rua','C',35,0})
        AADD(matriz,{'num','C',5,0})
        AADD(matriz,{'apto','C',5,0})
        AADD(matriz,{'bairro','C',20,0})
        AADD(matriz,{'cidade','C',20,0})
        AADD(matriz,{'estado','C',2,0})
        AADD(matriz,{'cep','C',8,0})
        AADD(matriz,{'mes_ano','C',8,0})
        AADD(matriz,{'ano','C',4,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\discalu.dbf',matriz)
ENDIF
**********************************************************************************************
DEVPOS(24,39);DEVOUT('ESTNOTA=>')
*IF ! FILE('\'+mdrive+'\'+mdiretorio+'\estnota.dbf')
IF ! FILE('\'+mdrive+'\estnota.dbf')
        matriz:={ }
        AADD(matriz,{'titulo1','C',40,0})
        AADD(matriz,{'gstt','C',4,0})
        AADD(matriz,{'unidade','C',2,0})
        AADD(matriz,{'disci','C',20,0})
        AADD(matriz,{'menor','N',10,2})
        AADD(matriz,{'entre','N',10,2})
        AADD(matriz,{'maior','N',10,2})
        DBCREATE('\'+mdrive+'\estnota.dbf',matriz)
ENDIF
****************************************************************************
DEVPOS(24,39);DEVOUT('cartacob=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cartacob.dbf')
        matriz:={ }
        AADD(matriz,{'lin1','C',73,0})
        AADD(matriz,{'lin2','C',73,0})
        AADD(matriz,{'lin3','C',73,0})
        AADD(matriz,{'lin4','C',73,0})
        AADD(matriz,{'lin5','C',73,0})
        AADD(matriz,{'lin6','C',73,0})
        AADD(matriz,{'lin7','C',73,0})
        AADD(matriz,{'lin8','C',73,0})
        AADD(matriz,{'lin9','C',73,0})
        AADD(matriz,{'lin10','C',73,0})
        AADD(matriz,{'lin11','C',73,0})
        AADD(matriz,{'lin12','C',73,0})
        AADD(matriz,{'lin13','C',73,0})
        AADD(matriz,{'lin14','C',73,0})
        AADD(matriz,{'lin15','C',73,0})
        AADD(matriz,{'lin16','C',73,0})
        AADD(matriz,{'lin17','C',73,0})
        AADD(matriz,{'lin18','C',73,0})
        AADD(matriz,{'lin19','C',73,0})
        AADD(matriz,{'lin20','C',73,0})
        AADD(matriz,{'lin21','C',73,0})
        AADD(matriz,{'lin22','C',73,0})
        AADD(matriz,{'lin23','C',73,0})
        AADD(matriz,{'lin24','C',73,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\cartacob.dbf',matriz)
ENDIF
****************************************************************************
DEVPOS(24,39);DEVOUT('cartadia=>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\cartadia.dbf')
        matriz:={ }
        AADD(matriz,{'lin1','C',73,0})
        AADD(matriz,{'lin2','C',73,0})
        AADD(matriz,{'lin3','C',73,0})
        AADD(matriz,{'lin4','C',73,0})
        AADD(matriz,{'lin5','C',73,0})
        AADD(matriz,{'lin6','C',73,0})
        AADD(matriz,{'lin7','C',73,0})
        AADD(matriz,{'lin8','C',73,0})
        AADD(matriz,{'lin9','C',73,0})
        AADD(matriz,{'lin10','C',73,0})
        AADD(matriz,{'lin11','C',73,0})
        AADD(matriz,{'lin12','C',73,0})
        AADD(matriz,{'lin13','C',73,0})
        AADD(matriz,{'lin14','C',73,0})
        AADD(matriz,{'lin15','C',73,0})
        AADD(matriz,{'lin16','C',73,0})
        AADD(matriz,{'lin17','C',73,0})
        AADD(matriz,{'lin18','C',73,0})
        AADD(matriz,{'lin19','C',73,0})
        AADD(matriz,{'lin20','C',73,0})
        AADD(matriz,{'lin21','C',73,0})
        AADD(matriz,{'lin22','C',73,0})
        AADD(matriz,{'lin23','C',73,0})
        AADD(matriz,{'lin24','C',73,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\cartadia.dbf',matriz)
ENDIF
****************************************************************************
DEVPOS(24,39);DEVOUT('NOBREGA =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\nobrega.dbf')
        matriz:={ }
        AADD(matriz,{'banseque','N',8,0})
        AADD(matriz,{'banumero','C',8,0})
        AADD(matriz,{'banomecl','C',40,0})
        AADD(matriz,{'banvalor','N',7,2})
        AADD(matriz,{'bandatap','D',8,0})
        AADD(matriz,{'banlinh1','C',60,0})
        AADD(matriz,{'banlinh2','C',60,0})
        AADD(matriz,{'banlinh3','C',60,0})
        AADD(matriz,{'banlinh4','C',60,0})
        AADD(matriz,{'banlinh5','C',60,0})
        AADD(matriz,{'banlinf1','C',80,0})
        AADD(matriz,{'banlinf2','C',80,0})
        AADD(matriz,{'banlinf3','C',80,0})
        AADD(matriz,{'conta','C',22,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\nobrega.dbf',matriz)
ENDIF
****************************************************************************
DEVPOS(24,39);DEVOUT('SAFCAIXA =>')
IF ! FILE('\'+mdrive+'\safcaixa.dbf')
        matriz:={ }
        AADD(matriz,{'ano','C',4,0})
        AADD(matriz,{'tipo','C',2,0})
        AADD(matriz,{'data','D',8,0})
        AADD(matriz,{'nota','C',8,0})
        AADD(matriz,{'num_dup','C',10,0})
        AADD(matriz,{'num_ban','C',3,0})
        AADD(matriz,{'cod_ct','C',3,0})
        AADD(matriz,{'documento','C',10,0})
        AADD(matriz,{'venci','D',8,0})
        AADD(matriz,{'valor','N',10,2})
        AADD(matriz,{'vlr_multa','N',10,2})
        AADD(matriz,{'vlr_juros','N',10,2})
        AADD(matriz,{'vlr_igpm','N',10,2})
        AADD(matriz,{'tipo_comp','C',2,0})
        AADD(matriz,{'coddesp','C',3,0})
        AADD(matriz,{'descri1','C',40,0})
        AADD(matriz,{'descri2','C',40,0})
        AADD(matriz,{'cod_vend','C',3,0})
        AADD(matriz,{'cod_cli','C',6,0})
        AADD(matriz,{'cod_opera','C',3,0})
        AADD(matriz,{'sangria','C',1,0})
        AADD(matriz,{'op_pg','C',3,0})
        AADD(matriz,{'pg','C',1,0})
        AADD(matriz,{'hora','C',8,0})
        AADD(matriz,{'autoriz','C',3,0})
        AADD(matriz,{'valor_com','N',10,2})
        AADD(matriz,{'comissao','N',12,6})
        AADD(matriz,{'tp_desp','C',1,0})
        AADD(matriz,{'num_pdv','C',4,0})
        AADD(matriz,{'num_cup','C',6,0})
        DBCREATE('\'+mdrive+'\safcaixa.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\safcaixa.cdx')
        IF ! AbriArq('safcaixa','caix','E');RETURN NIL;ENDIF
        INDEX ON DTOS(data) + num_dup + tipo + documento TAG data TO ('\'+mdrive+'\safcaixa')
        prog(24,50,20)
        INDEX ON cod_cli TAG codcli TO ('\'+mdrive+'\safcaixa')
        prog(24,50,30)
        INDEX ON tipo + documento + cod_cli + pg TAG tipo TO ('\'+mdrive+'\safcaixa')
        prog(24,50,40)
        INDEX ON descri1 + DTOS(data) + tipo TAG desc TO ('\'+mdrive+'\safcaixa')
        prog(24,50,50)
        INDEX ON num_ban + documento + STR(valor,10,2) TAG numban TO ('\'+mdrive+'\safcaixa')
        prog(24,50,60)
        INDEX ON num_dup+ano+tipo TAG nota TO ('\'+mdrive+'\safcaixa')
        prog(24,50,70)
        INDEX ON coddesp +tipo+ DTOS(data) TAG desp TO ('\'+mdrive+'\safcaixa')
        prog(24,50,80)
        INDEX ON DTOS(data) + tipo + ano + num_dup TAG datip TO ('\'+mdrive+'\safcaixa')
        prog(24,50,100)
        CLOSE caix
ENDIF
**********************************************************************************************
DEVPOS(24,39);DEVOUT('SAFCADDE =>')
IF ! FILE('\'+mdrive+'\safcadde.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',3,0})
        AADD(matriz,{'desc1','C',40,0})
        AADD(matriz,{'desc2','C',40,2})
        DBCREATE('\'+mdrive+'\safcadde.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\safcadde.cdx')
        IF ! AbriArq('safcadde','desp','E');RETURN NIL;ENDIF
        INDEX ON codigo TAG codigo TO ('\'+mdrive+'\safcadde')
        prog(24,50,50)
        INDEX ON desc1+desc2 TAG desc TO ('\'+mdrive+'\safcadde')
        prog(24,50,100)
        CLOSE desp
ENDIF
**********************************************************************************************
DEVPOS(24,39);DEVOUT('SAFIGPM  =>')
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safigpm.dbf')
        matriz:={ }
        AADD(matriz,{'mes','C',2,0})
        AADD(matriz,{'vlr_igpm','N',12,4})
        AADD(matriz,{'oper','C',3,0})
        DBCREATE('\'+mdrive+'\'+mdiretorio+'\safigpm.dbf',matriz)
ENDIF
IF ! FILE('\'+mdrive+'\'+mdiretorio+'\safigpm.cdx')
        IF ! AbriArq('safigpm','igpm','E');RETURN NIL;ENDIF
        INDEX ON mes TAG mes TO ('\'+mdrive+'\'+mdiretorio+'\safigpm')
        prog(24,50,100)
        CLOSE igpm
ENDIF
**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
limpa(24,00,24,79)
*atencao('Todos arquivos foram gerado com sucesso',.2)
RETURN NIL

