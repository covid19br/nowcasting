# Análises de _nowcasting_

## Estrutura do repositório

Repositório para análises de _nowcasting_ de COVID-19 a partir de microdados de SIVEP-Gripe.

    .
    ├── update_nowcasting.R    # Script principal para rodar o workflow
    ├── _src/                  # Scripts secundários para rodar as análises (source files)
    ├── fct/                   # Funções auxiliares
    │    
    ├── dados_processados/     # Dados com as saídas de nowcasting
    │    ├─ nowcasting/
    │    │    ├── municipios/
    │    │    │     └── [UF]/
    │    │    │         ├── [nome_municipio]/
    │    │    │         │      ├── output_nowcasting/
    │    │    │         │      ├── tabelas_nowcasting_para_grafico/
    │    │    │         │      └── plots/
    │    │    │         └── ...
    │    │    │                 
    │    │    ├── estados/
    │    │    |     ├── [UF]/
    │    │    │     └── ...
    │    │    │    
    │    │    └── ...  
    │    │
    │    └─ projecao_leitos/
    │         └── municipio_SP/
    │
    ├── .gitignore 
    │
    └── README.md

### Caminho para dados de entrada

Os dados de entrada são parametrizados no script `update_nowcasting.R` usando o argumento `dir`. 

### Caminho para outputs

Os outputs gerados por `update_nowcasting.R` vão para `dados_processados/nowcasting/[escala]/[sigla_UF]/[nome]`. Os dados processados de *nowcasting* estão na raiz desse caminho e há uma subpasta `dados_processados/nowcasting/[escala]/[sigla_UF]/[nome]/tabelas_nowcasting_para_grafico` onde são guardadas as tabelas de *nowcasting* consolidadas. 

## Para rodar os scripts

Veja [aqui](https://github.com/covid19br/nowcasting/tree/master/_src) o que está por trás do script `update_nowcasting.R`. 

## Dependências

Para executar o conjunto de scripts disponibilizados nesse repositório o usuário deverá instalar os seguintes programas:

- R versão >= 3.6.3
- JAGS versão 4.3.0

Além disso, a execução dos scripts em R depende da instalação dos pacotes a seguir:

- dplyr 
- EpiEstim 
- foreign
- lubridate 
- NobBS 
- patchwork 
- optparse 
- zoo

Construímos funções acessórias que estão em `fct` para facilitar o fluxo de trabalho.

## Sobre o método

- *Nowcasting* com método *Bayesian smoothing* de McGough et al (2019), usando a função `NobBS` do pacote R de mesmo nome (McGough et al 2020), com janela (argumento `moving_window`) de 40 dias para nowcasting diário

- *Nowcasting* diário até dois dias antes da última data de sintoma que consta na base. Este corte é feito
porque o _nowcasting_ diário tem muita variação nas previsões dos dias mais recentes, para os quais
normalmente há poucos casos

- As correções de *nowcasting* são feitas até a data mais recente de sintoma que consta em cada base e
para cada recorte. Por isso não necessariamente a correção chega até a data da versão da base

- Datas de início do evento para estimativa *nowcasting*: data do primeiro sintoma relatado (campo
`DT_SIN_PRI`)

- Data de registro do caso para estimativa *nowcasting* de casos SRAG: data de digitação (`DT_DIGITA`)

- Data de registro do caso para estimativa *nowcasting* de casos COVID: maior data entre data do resultado
do teste (`DT_PCR`) e de digitação (`DT_DIGITA`)

- Distribuição a priori de tempos de atraso (betas do modelo): binomial com média de atraso de 15 dias

- Cálculo dos tempos de duplicação a partir do limite superior do intervalo de credibilidade (95%) dos
estimados por *nowcasting*. Tempos de duplicação estimados com os usando os mesmos métodos do [site
do Observatório COVID-19 BR](https://covid19br.github.io)

- Cálculo dos tempos de R efetivo (R) a partir do limite superior do IC 95% dos valores estimados por
nowcating. R e calculado pelo método de Wallinga & Teunis (2004), implementado no pacote **EpiEstim**
(Cori et al 2020, função `estimate_R`). Utilizado o método que assume distribuição gama dos intervalos
seriais, com parâmetros tomados de Nishiura et al. (2020).

## Referências

Cori, Anne (2019). EpiEstim: Estimate Time Varying Reproduction Numbers from Epidemic Curves. R
  package version 2.2-1. https://CRAN.R-project.org/package=EpiEstim

McGough, S. F., Johansson, M. A., Lipsitch, M., & Menzies, N. A. (2020). Nowcasting by Bayesian Smoothing: A flexible, generalizable model for real-time epidemic tracking. PLoS computational biology, 16(4), e1007735.

McGough, S., Menzies, N., Lipsitch, M., Johansson, M. (2020). NobBS: Nowcasting by
  Bayesian Smoothing. R package version 0.1.0. https://CRAN.R-project.org/package=NobBS
  
  Nishiura, H., Linton, N. M., & Akhmetzhanov, A. R. (2020). Serial interval of novel coronavirus (COVID-19) infections. International journal of infectious diseases.
  
 Wallinga, J., & Teunis, P. (2004). Different epidemic curves for severe acute respiratory syndrome reveal similar impacts of control measures. American Journal of epidemiology, 160(6), 509-516.

