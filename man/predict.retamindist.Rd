% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrocompat.r
\name{predict.retamindist}
\alias{predict.retamindist}
\title{Amostragem De Pontos Na \code{retamindist}}
\usage{
\method{predict}{retamindist}(object, pontos, as.gradecolina = FALSE, ...)
}
\arguments{
\item{object}{objeto da classe \code{retamindist} retornado pela funcao homonima}

\item{pontos}{nao tem uso neste metodo, existe apenas para consistencia com os outros}

\item{as.gradecolina}{booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
interpolados nas coordenadas \code{pontos}; se \code{TRUE} um objeto \code{gradecolina}. Veja
\code{\link{gradecolina}}}

\item{...}{existe somente para consistencia de metodos. Nao possui utilidade}
}
\value{
vetor de rendimentos interpolados
}
\description{
Metodo dummy para consistencia com outras classes de interpolador
}
\details{
Os demais objetos gerados por \code{\link{interpolador}} sao modelos dos quais e possivel extrair
uma grade. Por outro lado, objetos \code{retamindist} \bold{ja sao interpolacoes} da curva colina
atraves do metodo vigente ate o Ciclo 2, implementado em VBA. Por esta razao seu \code{predict}
nao tem uso direto, apenas retornando a grade gerada.
}
