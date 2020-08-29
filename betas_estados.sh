#!/bin/bash

# lista de siglas e geocodes de estados
declare -A ESTADOS=([12]=AC [27]=AL [13]=AM [16]=AP [29]=BA [23]=CE [53]=DF [32]=ES [52]=GO [21]=MA [31]=MG [50]=MS [51]=MT [15]=PA [25]=PB [26]=PE [22]=PI [41]=PR [33]=RJ [24]=RN [11]=RO [14]=RR [43]=RS [42]=SC [28]=SE [35]=SP [17]=TO)

for VARIABLE in ${!ESTADOS[@]}
	do echo Rscript update_nowcasting.R --escala estado --geocode $VARIABLE --outputDir ./Estados_betas/estado_${ESTADOS[$VARIABLE]} --Rmethod Cori --betas TRUE
done | parallel --memfree 500M
wait
echo "all done"
