#!/bin/bash

for VARIABLE in 3304557 2611606 2927408 2304400 5300108

do
  Rscript update_nowcasting.R --escala municipio --geocode $VARIABLE --escala municipio --dir ../dados/SIVEP-Gripe
  Rscript update_nowcasting.R --escala municipio --geocode $VARIABLE --escala municipio --dir ../dados/SIVEP-Gripe --plot TRUE

done
