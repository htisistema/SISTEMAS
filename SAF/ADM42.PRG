*** ADM42.PRG: Reindexacao
***
FUNCTION adm42

set date ansi
save scree to adm42
quadro("CLIENTE1.NTX")
USE cliente
index on codigo to cliente1
use
rest screen from adm42
QUADRO("CLIENTE2.NTX")
USE cliente
index on razao to cliente2
use
rest screen from adm42
QUADRO("VISITA1.NTX")
USE visita
index on codigo+dtoc(data) to visita1.ntx
use
rest screen from adm42
QUADRO("VISITA2.NTX")
USE visita
index on dtoc(data)+codigo to visita2.ntx
use
rest screen from adm42
quadro("ATIV.NTX")
USE ativ
index on codigo to ativ.ntx
use
rest screen from adm42
quadro("VENDEDOR.NTX")
USE vendedor
index on codigo to vendedor.ntx
use
rest screen from adm42
quadro("REGIAO.NTX")
USE regiao
index on codigo to regiao.ntx
use
rest screen from adm42
quadro("PROPO1.NTX")
USE proposta
index on codigo+numprop to propo1.ntx
use
rest screen from adm42
quadro("PROPO2.NTX")
USE proposta
index on numprop+codigo to propo2.ntx
use
rest screen from adm42
close all
commit
set date brit
return

