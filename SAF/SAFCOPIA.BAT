ECHO OFF
ARJ A -G? -JT1 saf *.PRG *.CLP *.BAT *.dqy *.xls *.LNK
COPY saf.ARJ %1
PAUSE
