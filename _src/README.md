# Para gerar as análises de _nowcasting_


# 1. Para gerar os dados consolidados dos gráficos de _nowcasting_ para cada estado e município 

Os scripts rodam automaticamente a partir de `update_nowcasting.R`.

Em um terminal de bash a opção escala permite selecionar a escala de análise (estado, município) e a opção sigla permite indicar a sigla de duas letras de cada estado.



```bash
Rscript update_nowcasting.R --escala estado --sigla PB
```

Para testar localmente, executar `update_nowcasting.R` que chama os scripts seguintes: 

1. `01_gera_nowcasting_SIVEP.R` - executa e gera o nowcasting a partir de uma pasta de dados
1. `02_prepara_dados_nowcasting.R` - lê arquivos .csv em `dados_processados/nowcasting/[escala]/[sigla ou geocode]/[nome]/`
2. `03_analises_nowcasting.R` - gera objetos com projeções, R, TD e exporta arquivos no diretório local `para_o_site`



 
