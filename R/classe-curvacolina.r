####################################### FUNCOES PARA LEITURA #######################################

#' Le Planilha de Curva Colina
#' 
#' Funcoes para leitura de arquivos xlsx e extracao da curva colina em formato padronizado
#' 
#' \code{learqcolina} deve ser utilizada para leitura de arquivos especificos de curva colina, nos 
#' quais existe apenas uma aba contendo a informacao das curvas. \code{learqprocit} e especifica 
#' para uso com planilhas de processo iterativo, nas quais pode haver mais de uma curva colina.
#' 
#' Para \code{learqcolina}, \code{g} e \code{rho} permitem que sejam passados por fora a densidade e
#' gravidade no local. \code{learqprocit} ja busca esses valores por padrao na planilha de processo
#' iterativo, nao sendo necessario passa-los.
#' 
#' @param arq caminho da planilha
#' @param aba aba a ser lida. Por padrao igual a \code{1}, geralmente nao deve ser informado pelo 
#'     usuario
#' @param g,rho parametros opcionais contendo gravidade e densidade da agua no local. Ver Detalhes
#' 
#' @examples 
#' 
#' # arquivo de colina simples
#' arq <- system.file(package = "curvacolina", "extdata/colina.xlsx")
#' colina <- learqcolina(arq)
#' 
#' \dontrun{
#' plot(colina)
#' }
#' 
#' @return objetos \code{curvacolina}: lista de um elemento \code{data.table} contendo as colunas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{vaz}}{vazao turbinada}
#' \item{\code{rend}}{rendimento correspondente}
#' }
#' 
#' Adicionalmente tem os atributos:
#' 
#' \describe{
#' \item{\code{ncurvas}}{numero de curvas na colina}
#' \item{\code{rends}}{vetor de rendimentos contidos na curva colina}
#' \item{\code{max}}{rendimento no "olho" da colina}
#' \item{\code{rho}}{densidade da agua associada}
#' \item{\code{g}}{gravidade associada}
#' }
#' 
#' \code{leaqrcolina} retorna apenas um objeto, enquanto \code{learqprocit} retornara uma lista de
#' tantos elementos quanto abas de curva colina existem na planilha \code{arq}
#' 
#' @rdname leexcel
#' 
#' @family curvacolina
#' 
#' @export

learqcolina <- function(arq, aba = 1, g, rho) {

    if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Pacote 'readxl (>= 1.3.1)' necessario para leitura de arquivos de colina")
    }

    plan <- as.data.frame(readxl::read_xlsx(arq, aba, col_names = FALSE, .name_repair = "minimal"))

    if(nrow(plan) == 0) return(NULL)

    ncurvas <- (ncol(plan) + 1) / 3
    curvas  <- lapply(seq(ncurvas), function(i) {
        col1 <- 1 + (i - 1) * 3
        col2 <- col1 + 1

        rend <- plan[1, col1]
        rend <- regmatches(rend, regexpr("[[:digit:]]+(\\.[[:digit:]]+)?", rend))
        rend <- as.numeric(rend)

        curva <- plan[, col1:col2]
        curva <- curva[complete.cases(curva), ]
        curva[] <- lapply(curva, as.numeric)
        colnames(curva) <- c("hl", "pot")

        cbind(rend = rend, curva)
    })
    curvas <- do.call(rbind, curvas)

    new_curvacolina(curvas, g, rho)
}

#' @rdname leexcel
#' 
#' @export

learqprocit <- function(arq) {

    if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Pacote 'readxl (>= 1.3.1)' necessario para leitura de arquivos de processo iterativo")
    }

    hl <- pot <- vaz <- rend <- NULL

    abas <- readxl::excel_sheets(arq)
    abas_colina   <- abas[grep("Colina", abas)]
    abas_abertura <- abas[grep("Abertura", abas)]

    alterada <- grepl("Alterada", abas_colina)
    if(any(alterada)) abas_colina <- abas_colina[alterada]

    # algumas planilhas de procit com mais de uma colina tem rho e g em todas as aberturas e algumas
    # so tem na abertura (1). Pode ser um bug do codigo antigo
    rho_g <- as.data.table(readxl::read_xlsx(arq, abas_abertura[1], col_names = FALSE, .name_repair = "minimal"))
    rho_g <- as.numeric(rho_g[14:18, 7][[1]])
    rho_g <- if(all(is.na(rho_g[4:5]))) rho_g[1:2] else rho_g[4:5]
    rho_g <- lapply(abas_abertura, function(i) rho_g)

    colinas <- mapply(abas_colina, rho_g, FUN = function(a, r_g) {
        learqcolina(arq, a, r_g[1], r_g[2])
    }, SIMPLIFY = FALSE)

    # pode ser que alguma planilha tenha sido montada errado, com abas 'Alterada' vazias
    # nesse caso refaz a leitura das abas 'Original' mesmo, emitindo um aviso
    all_null <- all(sapply(colinas, is.null))
    if(all_null) {
        abas_colina <- abas[grepl("Colina Original", abas)]
        colinas <- mapply(abas_colina, rho_g, FUN = function(a, r_g) {
            learqcolina(arq, a, r_g[1], r_g[2])
        }, SIMPLIFY = FALSE)

        warning("Abas 'Alterada' em '", arq, "' foram encontradas vazias -- leitura realizada das 'Original'")
    }

    colinas <- colinas[!sapply(colinas, is.null)]

    if(length(colinas) > 1) names(colinas) <- paste0("colina_", seq(abas_colina)) else colinas <- colinas[[1]]

    return(colinas)
}

#' Construtor Interno Da Classe \code{curvacolina}
#' 
#' Construtor para uso interno, nao deve ser chamado diretamente pelo usuario
#' 
#' O dado \code{curvas} deve ser um data.table de tres colunas nomeadas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia na turbina}
#' \item{\code{rend}}{rendimento da turbina}
#' }
#' 
#' Conjuntamente com os valores de \code{rho} e \code{g} este dado e utilizado para calculo da 
#' vazao. Se nao forem passados, serao assumidos como NA e a vazao tambem sera NA. 
#' 
#' Caso ja exista uma coluna de vazao chamada \code{vaz}, ela sera preservada no dado. Caso sejam 
#' passados \code{rho} e \code{g} nesse caso eles ficarao incompativeis com os valores de vazao.
#' 
#' @param curvas data.table contendo a digitalizacao da colina. Ver Detalhes
#' @param g aceleracao da gravidade
#' @param rho massa especifica da agua
#' 
#' @return objeto \code{curvacolina}, uma lista de um elemento \code{data.table} contendo as colunas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{vaz}}{vazao turbinada}
#' \item{\code{rend}}{rendimento correspondente}
#' }
#' 
#' Adicionalmente tem os atributos:
#' 
#' \describe{
#' \item{\code{ncurvas}}{numero de curvas na colina}
#' \item{\code{rends}}{vetor de rendimentos contidos na curva colina}
#' \item{\code{max}}{rendimento no "olho" da colina}
#' \item{\code{rho}}{densidade da agua associada}
#' \item{\code{g}}{gravidade associada}
#' } 
#' 
#' @import data.table

new_curvacolina <- function(curvas, g, rho) {

    hl <- pot <- vaz <- rend <- NULL

    if(missing(rho)) rho <- NA
    if(missing(g)) g <- NA

    setDT(curvas)

    rends <- curvas[, unique(rend)]

    if(is.null(curvas$vaz) || all(is.na(curvas$vaz))) {
        curvas[, vaz := pot / (hl * rend / 100 * rho * g) * 1e6]
    }
    setcolorder(curvas, c("hl", "pot", "vaz", "rend"))
    curvas <- curvas[rend != 0]

    colina <- list(CC = curvas)

    temmax <- colina$CC[rend == max(rend), .N == 1]

    class(colina) <- c("curvacolina")
    attr(colina, "rends") <- rends
    attr(colina, "max") <- ifelse(temmax, colina$CC[which.max(rend), rend], NA)
    attr(colina, "ncurvas") <- length(rends)
    attr(colina, "rho") <- rho
    attr(colina, "g")   <- g

    return(colina)
}

# METODOS ------------------------------------------------------------------------------------------

#' @export 

print.curvacolina <- function(x, ...) summary(x)

#' Sumario De \code{curvacolina}
#'
#' Produz um breve sumario da curva colina
#' 
#' @param object objeto \code{curvacolina}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return imprime um sumario da curva colina, sem retornar nada
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

summary.curvacolina <- function(object, ...) {
    hl <- pot <- rend <- NULL
    cat("Numero de curvas:     ", attr(object, "ncurvas"), "\n")
    cat("Faixa de queda:       ", object$CC[, range(hl)], "\n")
    cat("Faixa de potencia:    ", object$CC[, range(pot)], "\n")
    cat("Faixa de rendimentos: ", range(attr(object, "rends")), "\n")
}

#' @export

`[.curvacolina` <- function(x, i, ...) {

    i <- substitute(i)
    i <- eval(i, envir = x$CC, enclos = parent.frame())
    CC <- x$CC[i]

    new_curvacolina(CC, attr(x, "g"), attr(x, "rho"))
}

#' @export

rbind.curvacolina <- function(...) {

    colinas <- list(...)

    comb <- lapply(colinas, "[[", "CC")
    comb <- rbindlist(comb)
    comb <- comb[!duplicated(comb)] # a curva associada a quebra existe nas duas colinas

    new_curvacolina(comb, attr(colinas[[1]], "g"), attr(colinas[[1]], "rho"))
}

#' Escrita De \code{curvacolina}
#' 
#' Metodo para facilitacao de escrita de \code{curvacolina} lida pelas funcoes do pacote
#' 
#' @param x objeto \code{curvacolina} a ser escrito
#' @param file caminho para escrita. Deve possuir extensao -- caso nao possua, sera adicionada
#' 
#' @return Escreve \code{x} no caminho especificado como um csv de quatro colunas
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

write.curvacolina <- function(x, file) fwrite(x$CC, file, quote = FALSE)

# HELPERS ------------------------------------------------------------------------------------------

#' Parser De Dados Para \code{as.curvacolina}
#' 
#' Funcao interna, nao deve ser chamada pelo usuario
#' 
#' Esta funcao executa todos os checks e coercoes necessarias para execucao da transformacao 
#' \code{as.curvacolina}
#' 
#' @param x \code{data.frame}-like a ser convertido
#' @param force booleano indicando se o objeto deve ser forcado ao formato padrao. Pode gerar 
#'     resultados inconsistentes. Ver Detalhes
#' 
#' @return dado parsed para transformacao

parsedadocolina <- function(x, force) {

    if(!inherits(x, "data.frame")) stop("Argumento deve ser um data.frame ou data.table")

    x <- as.data.frame(x)

    mincols <- c("hl", "pot", "rend")

    ncolsx <- ncol(x)
    temcolnames <- all(mincols %in% colnames(x))
    keepcols <- colnames(x)[match(c(mincols, "vaz"), colnames(x))]
    keepcols <- keepcols[!is.na(keepcols)]

    if(force) {
        if(!temcolnames) {
            if(ncolsx >= 3) {
                x <- x[, 1:3]
                warning("'x' nao possui todas as colunas 'hl', 'pot' e 'rend' -- tomando as tres primeiras")
                colnames(x) <- mincols
            } else if(ncolsx < 3) {
                stop("'x' possui menos de tres colunas -- abortando operacao")
            }
        } else {
            x <- x[, keepcols]
        }

        classes <- lapply(x, class)
        numerica <- sapply(classes, function(x) (x == "numeric") | (x == "integer"))

        if(!all(numerica)) {
            for(i in which(!numerica)) {
                v <- x[[i]]
                match <- regexpr("[[:digit:]]+(\\.[[:digit:]]+)?", v)
                match <- regmatches(v, match)

                if(length(match) == 0) stop("Nao foi possivel converter rendimentos para numerico")

                x[[i]] <- as.numeric(match)
            }

            warning("Transformando colunas para numerico")
        }

        if(!all(x$rend >= 1)) {
            x$rend <- x$rend * 100
            warning("Coluna de rendimento em formato decimal -- multiplicando por 100")
        }

    } else {
        if(!temcolnames) {
            stop("Verifique se as colunas 'hl', 'pot', 'rend' constam no dado")
        }

        x <- x[, keepcols]

        classes <- lapply(x, class)
        numerica <- sapply(classes, function(x) (x == "numeric") | (x == "integer"))

        if(!all(numerica)) stop("Alguma coluna de 'x' nao e numerica")

        if(!all(x$rend >= 1)) stop("Rendimentos estao em formato decimal -- multiplique por 100")
    }

    return(x)
}

#' Conversor Para \code{curvacolina}
#' 
#' Forca um objeto tipo \code{data.frame} para classe \code{curvacolina}
#' 
#' Por padrao a conversao de \code{x} so sera executada caso tres criterios sejam atendidos
#' 
#' \itemize{
#' \item{contenha as colunas \code{hl}, \code{pot} e \code{rend}}
#' \item{alguma das colunas nao esteja em formato numerico (ex: rendimento como "91%" ou "0.9%")}
#' \item{rendimentos nao estejam em formato decimal (ex: 0.91)}
#' }
#' 
#' A coluna de vazao nao e obrigatoria no dado, pois na maior parte das vezes sera calculada a 
#' partir da potencia. Caso ela exista, sera preservada na saida por padrao quando 
#' \code{force = FALSE}. Se \code{force = TRUE}, caso existam as demais colunas no dado, vazao sera
#' preservada, do contrario ela e descartada para ser recalculada.
#' 
#' Caso um ou mais destes requisitos nao seja atendido, por padrao a conversao sera abotada. Nestes 
#' casos, o argumento \code{force} permite forcar a transformacao do dado para \code{curvacolina}. A
#' seguir sao detalhados os comportamentos caso \code{force = TRUE} e algum criterio e violado.
#' 
#' Na falta \bold{de todas as tres colunas nomeadas}, o programa considera as tres primeiras colunas
#' do dado como \code{hl}, \code{pot} e \code{rend}, caso haja pelo menos tres colunas. Do contrario
#' aborta com um erro.
#' 
#' Se alguma coluna nao e numerica, sera tentada uma extracao de valores a partir do texto contido 
#' na coluna. Isto e feito buscando um padrao \code{"[[:digit:]]+(\\.[[:digit:]]+)?"} em cada 
#' elemento da coluna nao numerica. Desta forma, contanto que exista um numero dentro do texto (ex:
#' "91.5%" ou "rend_91.1"), sera possivel converter. Caso a conversao falhe, a funcao sera aborta.
#' 
#' Finalmente, se os rendimentos forem detectados como formato decimal, serao multiplicados por
#' 100. A identificacao e baseada em \bold{todos} os elementos da coluna serem menores do que 1.
#' 
#' E possivel que em algum momento desta coacao para o formato padrao ocorra algum erro, pois o 
#' programa nao esta preparado para lidar com todas as possibilidades de dado de entrada. Pelo mesmo
#' motivo, e possivel que a execucao termine sem erro, porem com resultados incoerentes. Desta forma
#' o uso da opcao \code{force = TRUE} e pouco recomendado, exceto em casos especificos.
#' 
#' @param x \code{data.frame}-like a ser convertido. Ver Detalhes
#' @param g,rho aceleracao da gravidade e densidade da agua
#' @param force booleano indicando se o objeto deve ser forcado ao formato padrao. Pode gerar 
#'     resultados inconsistentes. Ver Detalhes
#' 
#' @examples 
#' 
#' # gerando um dado totalmente aleatorio (nao condiz com uma colina de fato)
#' dd <- data.frame(hl = runif(100), pot = runif(100), rend = rep(1:10, each = 10))
#' cc <- as.curvacolina(dd, g = 9.81, rho = 1000)
#' 
#' @return objeto da classe \code{curvacolina}
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

as.curvacolina <- function(x, g = NA, rho = NA, force = FALSE) {

    hl <- pot <- rend <- NULL

    x <- parsedadocolina(x, force)
    x <- as.data.table(x)

    new_curvacolina(x, g, rho)
}

#' Redutor De \code{curvacolina}
#' 
#' Retorna o objeto \code{curvacolina} com um numero reduzido de observacoes
#' 
#' O argumento \code{taxa} e utilizado da seguinte maneira: seja \code{n} o numero de observacoes em
#' \code{colina}, sera amostrado um subset dessas tomando uma a cada \code{taxa} observacoes. Por
#' exemplo, se \code{taxa = 3}, sao selecionadas as observacoes 1, 4, 7, e assim por diante.
#' 
#' O processo de selecao descrito acima e realizado \bold{por curva de rendimento}, dado que a curva
#' tenha pelo menos \code{5 * taxa} observacoes. Este limite e algo essencialmente arbitrario.
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' @param taxa inteiro indicando reducao do tamanho do historico em \code{taxa_reducao} vezes. Ver 
#'     Detalhes
#' 
#' @return \code{colina} passada com observacoes selecionadas
#' 
#' @export

reduzcolina <- function(colina, taxa) {

    vec_reduz <- rep(FALSE, taxa)
    vec_reduz[1] <- TRUE

    colreduzida <- copy(colina$CC)
    colreduzida <- split(colreduzida, colreduzida$rend)
    colreduzida <- lapply(colreduzida, function(d) {
        if(nrow(d) > (5 * taxa)) {
            d <- d[rep(vec_reduz, length.out = .N)]
        }
        d
    })
    colreduzida <- as.curvacolina(rbindlist(colreduzida), attr(colina, "g"), attr(colina, "rho"))

    return(colreduzida)
}

#' Atribui \code{g} E \code{rho} A Objeto \code{curvacolina}
#' 
#' Adiciona os atributos e calcula vazao turbinada na transformacao de curva colina
#' 
#' @param x objeto da classe \code{curvacolina}
#' @param g,rho aceleracao da gravidade e densidade da agua
#' 
#' @examples 
#' 
#' \dontrun{
#' # o dado padrao do pacote nao possui g e rho
#' is.na(attr(colinadummy, "g"))
#' is.na(attr(colinadummy, "rho"))
#' }
#' 
#' # adicionando os valores de literatura
#' cc <- set_grho(colinadummy, 9.81, 1000)
#' 
#' \dontrun{
#' # atributos adicionados
#' attr(cc, "g")
#' attr(cc, "rho")
#' 
#' # vazao calculada
#' cc$CC$vaz
#' }
#' 
#' @return objeto \code{curvacolina} passado com atributos \code{g} e \code{rho}, assim como vazao
#'     turbinada caclulada
#' 
#' @export

set_grho <- function(x, g, rho) {

    if(!inherits(x, "curvacolina")) stop("'x' nao e um objeto 'curvacolina'")

    out <- x

    # aqui nao e usado a mod inplace do data.table para evitar efeitos colaterais em x no ambiente
    # global
    out$CC$vaz <- out$CC$pot / (out$CC$hl * out$CC$rend / 100 * rho * g) * 1e6

    attr(out, "g")   <- g
    attr(out, "rho") <- rho

    return(out)
}