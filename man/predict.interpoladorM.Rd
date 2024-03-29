% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod-interpoladorM.r
\name{predict.interpoladorM}
\alias{predict.interpoladorM}
\title{Amostragem De Pontos Em \code{interpoladorM}}
\usage{
\method{predict}{interpoladorM}(object, pontos, as.gradecolina = FALSE, ...)
}
\arguments{
\item{object}{objeto da classe \code{interpoladorM}}

\item{pontos}{data.frame ou matriz contendo coordenadas \code{(hl, pot)} dos pontos onde
interpolar}

\item{as.gradecolina}{booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
interpolados nas coordenadas \code{pontos}; se \code{TRUE} um objeto \code{gradecolina}. Veja
\code{\link{gradecolina}}}

\item{...}{existe somente para consistencia de metodos. Nao possui utilidade}
}
\value{
se \code{as.gradecolina = FALSE}, vetor de rendimentos interpolados, do contrario um
objeto \code{\link{gradecolina}}
}
\description{
Realiza a amostragem nos dois interpoladores contidos no objeto pelos metodos especificos
}
