% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classe-gradecolina.r
\name{interpolacao_bilinear}
\alias{interpolacao_bilinear}
\alias{predict.gradecolina}
\alias{fitted.gradecolina}
\alias{residuals.gradecolina}
\title{Interpolacao Bilinear}
\usage{
\method{predict}{gradecolina}(object, pontos, full.output = FALSE, ...)

\method{fitted}{gradecolina}(object, full.output = FALSE, ...)

\method{residuals}{gradecolina}(object, ...)
}
\arguments{
\item{object}{objeto da classe \code{gradecolina}}

\item{pontos}{data.frame ou matriz contendo pontos nos quais amostrar o rendimento}

\item{full.output}{booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
a coluna \code{rend} adicionada}

\item{...}{existe somente para consistencia de metodos. Nao possui utilidade}
}
\value{
set \code{full.output = FALSE} vetor de rendimentos interpolados, do contrario um
\code{data.table} contendo \code{pontos} adicionado da coluna \code{rend} com resultado da
interpolacao
}
\description{
Interpolacao bilinear de \code{pontos} na grade bivariada \code{gradecolina}
}
\details{
\code{predict.gradecolina} interpola pontos arbitrarios especificados atraves do argumento
\code{pontos}. \code{fitted.gradecolina} interpola os pontos da propria curva colina original
na grade. \code{residuals.gradecolina} retorna os erros de interpolacao dos pontos da colina
original na grade.
}
\examples{

# usando o interpolador de triangulacao
tri <- interpolador(colinadummy, "triangulacao")

# extrai uma grade dele
coord <- coordgrade(colinadummy, 10, 10)
gradecolina <- predict(tri, coord, as.gradecolina = TRUE)

# interpolando pontos arbitrarios
coord_interp <- coordgrade(colinadummy, 25, 25)
pred <- predict(gradecolina, coord_interp)
pred <- predict(gradecolina, coord_interp, full.output = TRUE)

# interpolando a propria curva colina
fitt <- fitted(gradecolina)

# residuos
resid <- residuals(gradecolina)

}
\seealso{
Other gradecolina: 
\code{\link{gradecolina}},
\code{\link{plot.gradecolina}()},
\code{\link{write.gradecolina}()}
}
\concept{gradecolina}
