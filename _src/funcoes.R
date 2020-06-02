library(zoo)
library(EpiEstim)
library(lubridate)

# carraga funcoes em fct, listagem em ordem alfabetica
source("fct/beta.summary.R")
source("fct/check.geocode.R")
source("fct/diazero.R")
source("fct/dt.rw.R")
source("fct/estima.not.R")
source("fct/estimate.R0.R")
source("fct/existe.nowcasting.R")
source("fct/existe.nowcasting2.R")
source("fct/fitP.exp.R")
source("fct/forecast.exponential.R")
source("fct/formata.now.df.R")
source("fct/gera.nowcasting.R")
source("fct/get.data.base.R")
source("fct/get.last.date.R")
source("fct/inv.logit.R")
source("fct/makeNamedList.R")
source("fct/na.zero.R")
source("fct/NobBS.posterior.R")
source("fct/now.proj.R")
source("fct/preenche.now.R")
source("fct/prepara.dados.R")
source("fct/read.sivep.R")
source("fct/Re.com.data.R")
source("fct/tabelas.web.R")
source("fct/write.notificacoes.data.R")
source("fct/write.nowcasting.R")
source("fct/zoo2df.R")

# formatacao e funcoes de plot
source("fct/plot.formatos.R")
source("fct/plot.nowcast.diario.R")
source("fct/plot.nowcast.acumulado.R")
source("fct/plot.estimate.R0.R")
source("fct/plot.tempo.dupl.R")


################################################################################
## Funcoes das rotinas de projecao de leitos
################################################################################

source("fct/age_table.R")
source("fct/wait_times.R")
source("fct/fix.dates.R")
source("fct/fillNowcastedLines.R")
source("fct/get.hospital.probabilities.R")
source("fct/get.times.R")
source("fct/countByAgeClass.R")
source("fct/getCurrentInBed.R")
source("fct/makeHospitalTable.R")
source("fct/plotTimesValidation.R")

