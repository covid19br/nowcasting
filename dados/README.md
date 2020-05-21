# Geocode de municipios, micro, mesorregiões e estados

O arquivo `geocode_ibge.csv` foi gerado com o script abaixo. São necessários os pacotes **jsonlite** e **textclean**. Qualquer modificação documentar aqui.

```
url <- paste0("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")
df <- jsonlite::fromJSON(url)
df$nome.nonascii <- gsub(" ", "_", textclean::replace_non_ascii(df$nome))
df$nome.nonascii <- gsub("'", "", df$nome.nonascii)
write.csv(df, "geocode_ibge.csv", row.names = FALSE)
```
