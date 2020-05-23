# Para gerar as análises de _nowcasting_


# 1. Para gerar os dados consolidados dos gráficos de _nowcasting_ para cada estado e município 

Os scripts rodam automaticamente a partir de `update_nowcasting.R`.

Em um terminal de bash a opção escala permite selecionar a escala de análise (estado, município) e a opção geocode especifica a unidade administrativa de interesse. O exemplo abaixo é para o município do Rio de Janeiro, RJ. 

```bash
  Rscript update_nowcasting.R --escala municipio --geocode 3304557 --escala municipio --dir ../dados/SIVEP-Gripe
```

Em seguida, se quiser rodar os plots é só rodar o script bash abaixo que é o mesmo do anterior com a opção de plot.

```bash
  Rscript update_nowcasting.R --escala municipio --geocode 3304557 --escala municipio --dir ../dados/SIVEP-Gripe --plot TRUE
```

Para testar localmente, executar `update_nowcasting.R` que chama os scripts seguintes: 

1. `01_gera_nowcasting_SIVEP.R` - executa e gera o nowcasting a partir de uma pasta de dados
1. `02_prepara_dados_nowcasting.R` - lê arquivos .csv em `dados_processados/nowcasting/[escala]/[sigla ou geocode]/[nome]/`
2. `03_analises_nowcasting.R` - gera objetos com projeções, R, TD e exporta arquivos no diretório local `para_o_site`
4. `04_plots_nowcasting.R` - gera plots a partir das tabelas consolidadas



 
