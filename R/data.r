###################################### DOCUMENTACAO DOS DADOS ######################################

#' Objeto \code{curvacolina} Exemplo
#' 
#' Exemplo de objeto retornado por uma das funcoes de leitura \code{\link{learqcolina}} ou 
#' \code{\link{learqprocit}}
#' 
#' @format Lista de um elemento chamado \code{CC}, um data.table de 1888 com as colunas
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{rend}}{rendimento correspondente}
#' }
#' 
#' Adicionalmente, contem os atributos 
#' 
#' \describe{
#' \item{\code{ncurvas}}{numero de curvas na colina}
#' \item{\code{rends}}{vetor de rendimentos contidos na curva colina}
#' \item{\code{max}}{rendimento no "olho" da colina}
#' \item{\code{rho}}{densidade da agua associada}
#' \item{\code{g}}{gravidade associada}
#' }
"colinadummy"