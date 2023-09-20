@echo off
cd\saf
md %2
cd\saf\%1
copy caddisc.dbf \saf\%2
copy cadprof.dbf \saf\%2
copy faixa.dbf \saf\%2
copy insopera.dbf \saf\%2
copy safprn.dbf \saf\%2
copy safsetup.dbf \saf\%2
copy tabgstt.dbf \saf\%2
copy tabmensa.dbf \saf\%2
cd..\%2
del *.cdx
cd..


