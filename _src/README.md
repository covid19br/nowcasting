# Para gerar as análises de _nowcasting_

## 0. Antes de fazer a análise

Para os dados públicos em `SIVEP-Gripe`, é possível seguir os passos abaixo.

- Os arquivos de _nowcasting_ devem ter sido gerados usando o script `gera_nowcastings_SIVEP.R`

Num terminal de bash: 

```bash
Rscript gera_nowcastings_SIVEP.R --dataBase 2020-05-04 --file SRAGHospitalizado_2020_05_04.csv --escala [estado]
```


# 1. Para gerar os dados consodidados de os gráficos de _nowcasting_ para cada estado e município 

Os scripts rodam automaticamente a partir de `update_nowcasting.R`.

Em um terminal de bash a opção escala permite selecionar a escala de análise (estado, município) e a opção sigla permite indicar a sigla de duas letras de cada estado.

```bash
Rscript update_nowcasting.R --escala municipio --sigla SP
Rscript update_nowcasting.R --escala estado --sigla PB
```

Para testar localmente, executar `update_nowcasting.R` que chama os scripts seguintes: 

1. `prepara_nowcasting.R` - lê arquivos .csv em `dados/estado_*`
2. `analises_nowcasting.R` - gera objetos com projeções, R, TD e exporta arquivos no diretório local `para_o_site`



 