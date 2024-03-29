% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{gerarange}
\alias{gerarange}
\title{Gera Vetor De Secoes Num Dominio}
\usage{
gerarange(vec, divs, by, expande)
}
\arguments{
\item{vec}{um vetor numerico contendo o conjunto de pontos nos quais se gera a segmentacao}

\item{divs}{inteiro indicando numero de divisoes. Ou este ou \code{by} deve ser informado}

\item{by}{intervalo entre divisoes. Ou este ou \code{divs} deve ser informado}

\item{expande}{vetor de duas posicoes indicando percentual de expansao do dominio}
}
\value{
vetor de divisoes no dominio de \code{vec} de acordo com os parametros passados
}
\description{
Funcao auxiliar para gerar uma sequencia de pontos de segmentacao num determinado vetor
}
\examples{

\dontrun{
gerarange(1:20, 5, expande = c(0, 0))
# [1]  1.00  5.75 10.50 15.25 20.00

gerarange(1:20, by = 5, expande = c(0, 0))
# [1]  0  5 10 15 20

gerarange(1:20, by = 5, expande = c(0.1, 0.1))
# [1] -5  0  5 10 15 20 25
}

}
