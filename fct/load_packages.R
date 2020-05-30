if(!require(plyr)){install.packages("plyr"); library(plyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(rstan)){install.packages("rstan"); library(rstan)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(rmarkdown)){install.packages("rmarkdown"); library(rmarkdown)}
if(!require(kableExtra)){install.packages("kableExtra"); library(kableExtra)}
if(!require(knitr)){install.packages("knitr"); library(knitr)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(brms)){install.packages("brms"); library(brms)}
if(!require(doMC)){install.packages("doMC"); library(doMC)}
if(!require(cowsay)){install.packages("cowsay"); library(cowsay)}
if(!require(aweek)){install.packages("aweek"); library(aweek)}
options(mc.cores = parallel::detectCores())
