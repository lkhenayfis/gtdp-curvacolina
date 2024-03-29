% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod-thinplate.r
\name{predict.thinplate}
\alias{predict.thinplate}
\title{Amostragem De Pontos Na Suavizacao}
\usage{
\method{predict}{thinplate}(object, pontos, as.gradecolina = FALSE, ...)
}
\arguments{
\item{object}{objeto da classe \code{thinplate} retornado pela funcao homonima}

\item{pontos}{data.frame ou matriz contendo coordenadas dos pontos onde interpolar}

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
Amostra as coordenadas especificadas via \code{pontos} na superficie suavizada
}
