************************************************
* CONSULTA DE ESTATISTICA DE NOTAS
************************************************

FUNCTION saf42
***************
LOCAL mprg:='SAF42'
LOCAL lci,cci,lba,cba,opcao,tela,i,mgstt,msele,morde,mpoint,mdisc
LOCAL mtot_aluno,mtot_nota,mtraco,mpag,mtit,muni

PRIVATE m_nota := {},munidade

IF ! ver_nivel(mprg,'CONSULTA DE ESTATISTICA DE NOTAS','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('estnota','est','E',' ');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 09
cci := 08
lba := 14
cba := 70
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        *****************
        SELE('nota');ORDSETFOCUS(3)
        GO TOP
        *****************
        ASIZE(m_nota,0)
        munidade := SPACE(2)
        mgstt := SPACE(4)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+3,cci+1 TO lci+3,cba-1
        janela(lci,cba,' Estatistica de Notas ','*')
        DEVPOS(lci+1,cci+1);DEVOUT('GSTT...:')
        DEVPOS(lci+2,cci+1);DEVOUT('Unidade:')
        DEVPOS(lci+4,cci+1);DEVOUT('Opcao: [U1] [U2] [3U] [4U] [MU] [MA] [MF]')
        setcor(1)
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        @ lci+1,cci+10 GET mgstt PICT '@!'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF ! nota->(DBSEEK(mgstt))
                atencao('Nao foi possivel encontrar esta TURMA')
                LOOP
        ENDIF
        @ lci+2,cci+10 GET munidade PICT '@!' VALID munidade $ 'U1,U2,U3,U4,MU,MA,MF'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF munidade = 'U1'
                muni := '1a. Unidade'
        ELSEIF munidade = 'U2'
                muni := '2a. Unidade'
        ELSEIF munidade = 'U3'
                muni := '3a. Unidade'
        ELSEIF munidade = 'U4'
                muni := '4a. Unidade'
        ELSEIF munidade = 'MU'
                muni := 'Media da Unidade'
        ELSEIF munidade = 'MA'
                muni := 'Media da Anual'
        ELSEIF munidade = 'MF'
                muni := 'Media da Final'
        ENDIF
        mensagem('Opcao: [I]mpressora [V]ideo:')
        opcao = 'V'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'I,V'
        READ
        IF opcao = 'N'
                LOOP
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
        IF opcao = 'I'
                SET PRINT TO 'LPT1'
        ELSE
                SET PRINT TO consulta.rel
        ENDIF
        mtot_aluno := 0
        mtot_nota := 0
        i := 0
        WHILE mgstt == nota->gstt .AND. ! EOF()
                mdisc := nota->cod_disc
                AADD(m_nota,{nota->cod_disc,ver_disc(nota->cod_disc),0,0,0,0,0,0,0})
                i ++
                WHILE nota->cod_disc = mdisc .AND. ! EOF()
                        IF munidade = 'U1'
                                IF nota->media_1 < 6
                                        m_nota[i,3] ++
                                        m_nota[i,4] := m_nota[i,4] + nota->media_1
                                ELSEIF nota->media_1 > 6        // 7
                                        m_nota[i,7] ++
                                        m_nota[i,8] := m_nota[i,8] + nota->media_1
                                ELSE
                                        m_nota[i,5] ++
                                        m_nota[i,6] := m_nota[i,6] + nota->media_1
                                ENDIF
                                m_nota[i,9] ++
                                mtot_nota := mtot_nota + nota->media_1
                        ELSEIF munidade = 'U2'
                                IF nota->media_2 < 6
                                        m_nota[i,3] ++
                                        m_nota[i,4] := m_nota[i,4] + nota->media_2
                                ELSEIF nota->media_2 > 6        // 7
                                        m_nota[i,7] ++
                                        m_nota[i,8] := m_nota[i,8] + nota->media_2
                                ELSE
                                        m_nota[i,5] ++
                                        m_nota[i,6] := m_nota[i,6] + nota->media_2
                                ENDIF
                                m_nota[i,9] ++
                                mtot_nota := mtot_nota + nota->media_2
                        ELSEIF munidade = 'U3'
                                IF nota->media_3 < 6
                                        m_nota[i,3] ++
                                        m_nota[i,4] := m_nota[i,4] + nota->media_3
                                ELSEIF nota->media_3 > 6        // 7
                                        m_nota[i,7] ++
                                        m_nota[i,8] := m_nota[i,8] + nota->media_3
                                ELSE
                                        m_nota[i,5] ++
                                        m_nota[i,6] := m_nota[i,6] + nota->media_3
                                ENDIF
                                m_nota[i,9] ++
                                mtot_nota := mtot_nota + nota->media_3
                        ELSEIF munidade = 'U4'
                                IF nota->media_4 < 6
                                        m_nota[i,3] ++
                                        m_nota[i,4] := m_nota[i,4] + nota->media_4
                                ELSEIF nota->media_4 > 6        //7
                                        m_nota[i,7] ++
                                        m_nota[i,8] := m_nota[i,8] + nota->media_4
                                ELSE
                                        m_nota[i,5] ++
                                        m_nota[i,6] := m_nota[i,6] + nota->media_4
                                ENDIF
                                m_nota[i,9] ++
                                mtot_nota := mtot_nota + nota->media_4
                        ENDIF
                        mtot_aluno ++
                        SKIP
                ENDDO
        ENDDO
        *********************
        SELE('est')
        *********************
        ZAP
        i := 0
        mpag := 1
        mtit := 'Estatistica de Notas do GSTT: '+mgstt+' da unidade: '+munidade
        cabecalho(mpag,mtit)
        imprt('M','C',1)
        DEVPOS(PROW(),00);DEVOUT('    ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('                   ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('      Menor que 6 ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('        Entre 6 e 7      ')
*       DEVPOS(PROW(),PCOL()+2);DEVOUT('        Igual a 6        ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Maior que 6      ')
        DEVPOS(PROW()+1,00);DEVOUT('Cod.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Disciplina          ')
        DEVPOS(PROW(),PCOL()+4);DEVOUT(' Qtd.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('   (%)')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Md.Nt.')
        DEVPOS(PROW(),PCOL()+4);DEVOUT(' Qtd.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('   (%)')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Md.Nt.')
        DEVPOS(PROW(),PCOL()+4);DEVOUT(' Qtd.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('   (%)')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Md.Nt.')
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        imprt('M','C')
        FOR i = 1 TO LEN(m_nota)
                DEVPOS(PROW()+1,02);DEVOUT(m_nota[i,1])
                DEVPOS(PROW(),PCOL()+2);DEVOUT(m_nota[i,2])
                DEVPOS(PROW(),PCOL()+4);DEVOUTPICT(m_nota[i,3],'9,999')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT((m_nota[i,3]/m_nota[i,9])*100,'999.99')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(m_nota[i,4]/m_nota[i,3],'999.99')

                DEVPOS(PROW(),PCOL()+4);DEVOUTPICT(m_nota[i,5],'9,999')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT((m_nota[i,5]/m_nota[i,9])*100,'999.99')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(m_nota[i,6]/m_nota[i,5],'999.99')

                DEVPOS(PROW(),PCOL()+4);DEVOUTPICT(m_nota[i,7],'9,999')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT((m_nota[i,7]/m_nota[i,9])*100,'999.99')
                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(m_nota[i,8]/m_nota[i,7],'999.99')
                ADIREG()
                est->titulo1 := 'GSTT: '+mgstt+' da '+muni+' do ano: '+mdiretorio
                est->gstt    := mgstt
                est->disci   := m_nota[i,2]
                est->menor   := (m_nota[i,3]/m_nota[i,9])*100   //m_nota[i,4]/m_nota[i,3]
                est->entre   := (m_nota[i,5]/m_nota[i,9])*100   //m_nota[i,6]/m_nota[i,5]
                est->maior   := (m_nota[i,7]/m_nota[i,9])*100   //m_nota[i,8]/m_nota[i,7]
                DBCOMMIT()
        NEXT
        EJECT
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF opcao = 'V'
                CAIXA(mold,02,00,MAXROW()-2,79,,,.T.)
                janela(02,79,' Consulta Fechamento do CAIXA ')
                mensagem('<TAB>p/Direita - <HOME>p/Esquerda - <ESC>p/abandonar')
                mconsulta := MEMOREAD('consulta.rel')
                MEMOEDIT(mconsulta,03,01,MAXROW()-3,78,.F.,,160,80)
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        LOOP
ENDDO

