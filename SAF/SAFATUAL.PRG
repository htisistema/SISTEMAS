
FUNCTION safatual(mdir)

PUBLIC mdiretorio,mdrive

REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
SET EPOCH TO 1911
mdrive := CURDIR()
mdiretorio := mdir
*--------------------------------------------------------------------------
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('cdmen');ORDSETFOCUS(1)
GO TOP
**************
BLOQARQ()
DEVPOS(00,00);DEVOUT('Registro sendo Processado:')
WHILE ! EOF()
        DEVPOS(00,27);DEVOUT(cdmen->matricula)
        IF ! ver_aluno(cdmen->matricula,'*')
                SKIP
                LOOP
        ENDIF
        IF mdiretorio <> cdmen->ano .AND. EMPTY(data_pag) .AND. cdmen->tipo = 'ME'
                IF cdmen->parcela = '01'
                        cdmen->bolsa := aluno->faixa_1
                        mdesconto := faixa_vlr(aluno->faixa_1)
                ELSEIF cdmen->parcela = '02'
                        cdmen->bolsa := aluno->faixa_2
                        mdesconto := faixa_vlr(aluno->faixa_2)
                ELSEIF cdmen->parcela = '03'
                        cdmen->bolsa := aluno->faixa_3
                        mdesconto := faixa_vlr(aluno->faixa_3)
                ELSEIF cdmen->parcela = '04'
                        cdmen->bolsa := aluno->faixa_4
                        mdesconto := faixa_vlr(aluno->faixa_4)
                ELSEIF cdmen->parcela = '05'
                        cdmen->bolsa := aluno->faixa_5
                        mdesconto := faixa_vlr(aluno->faixa_5)
                ELSEIF cdmen->parcela = '06'
                        cdmen->bolsa := aluno->faixa_6
                        mdesconto := faixa_vlr(aluno->faixa_6)
                ELSEIF cdmen->parcela = '07'
                        cdmen->bolsa := aluno->faixa_7
                        mdesconto := faixa_vlr(aluno->faixa_7)
                ELSEIF cdmen->parcela = '08'
                        cdmen->bolsa := aluno->faixa_8
                        mdesconto := faixa_vlr(aluno->faixa_8)
                ELSEIF cdmen->parcela = '09'
                        cdmen->bolsa := aluno->faixa_9
                        mdesconto := faixa_vlr(aluno->faixa_9)
                ELSEIF cdmen->parcela = '10'
                        cdmen->bolsa := aluno->faixa_10
                        mdesconto := faixa_vlr(aluno->faixa_10)
                ELSEIF cdmen->parcela = '11'
                        cdmen->bolsa := aluno->faixa_11
                        mdesconto := faixa_vlr(aluno->faixa_11)
                ELSEIF cdmen->parcela = '12'
                        cdmen->bolsa := aluno->faixa_12
                        mdesconto := faixa_vlr(aluno->faixa_12)
                ENDIF
                IF ! EMPTY(mdesconto)
                        cdmen->desc_bolsa := cdmen->vlr_parc * (mdesconto/100)
                ENDIF
        ENDIF
        cdmen->contrato := aluno->contrato
        SKIP
ENDDO
DBCOMMITALL()
DBUNLOCKALL()
*APPEND FROM ('\'+mdrive+'\'+ALLTRIM(mdir)+'\CDMENSAL.DBF')      //FOR EMPTY(DATA_PAG)
*COMMIT
*UNLOCK
RETURN NIL
