#' Conta número de linhas de um arquivo de texto, zipado ou bzipado ou xzeado
#' @param filename Caractere. Caminho do arquivo
count.lines <- function(filename){
    if (endsWith(filename, 'csv'))
        return(as.integer(system2('wc',
                                  args=c('-l', filename, ' | cut -d" " -f1'),
                                  stdout=T)))
    else if (endsWith(filename, 'zip'))
        return(as.integer(system2('unzip', args=c('-c', filename, '| wc -l'),
                                  stdout=T)))
    else if (endsWith(filename, 'bz2'))
        return(as.integer(system2('bzgrep', args=c('-c', '$', filename),
                                  stdout=T)))
    else if (endsWith(filename, 'xz'))
        return(as.integer(system2('xzgrep', args=c('-c', '$', filename),
                                  stdout=T)))
    else {
        print("Tipo de arquivo desconhecido")
        return(0)
    }
}
