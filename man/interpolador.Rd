% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolacao.r
\name{interpolador}
\alias{interpolador}
\title{Modelo Para Interpolacao De Curva Colina}
\usage{
interpolador(colina, metodo, quebra, modo = "pot", ...)
}
\arguments{
\item{colina}{objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)}

\item{metodo}{um ou dois de \code{c("triangulacao", "thinplate", "tensorpro")}. Ver Detalhes}

\item{quebra}{opcional, numerico indicando o rendimento da curva a partir da qual chavear
metodos. Ver Detalhes}

\item{modo}{um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
modelada}

\item{...}{demais parametros que possam ser passados as funcoes de ajuste de cada \code{metodo}.
Ver Detalhes}
}
\value{
objeto da classe \code{interpolador} e subclasse \code{metodo}, isto e, um modelo com o
qual se realizar previsoes e, assim, interpolar o dado original
}
\description{
Funcao para estimacao de diferentes interpoladores de curva colina
}
\details{
\code{interpolador} serve como uma interface comum para estimacao de diversas formas atraves das
quais interpolar a colina original. Atualmente ha tres abordagens implementadas:

\itemize{
\item{\code{\link{triangulacao}}}
\item{\code{\link{thinplate}}}
\item{\code{\link{tensorprod}}}
}

Se \code{metodo = "triangulacao"}, a colina fornecida e projetada no plano hl x pot/vaz e entao
tesselada atraves da triangulacao de Delaunay. Interpolacao de um ponto em objetos de
triangulacao se da atraves de transformacao para coordenadas baricentricas e subsequente media
ponderada dos rendimentos nos vertices do triangulo que o contem.

Quando \code{metodo = "thinplate"} ou \code{metodo = "tensorprod"}, a colina e suavizada atraves
de splines, cujo tipo depende do \code{metodo} passado: \code{"thinplate"} corresponde a
suavizacao por splines homonimas e \code{"tensorprod"} ao produto tensor de splines. A
interpolacao nesse caso consiste simplesmente da consulta a curva suavizada em pontos
selecionados.

O argumento \code{...} permite passar os argumentos opcionais aos metodos especificos. As paginas
de ajuda dos interpoladores proveem mais informacao a respeito destes argumentos extras.

\bold{Interpoladores Multiplos}:

Existe a possibilidade de estimar um interpolador combinando mais de um metodo. Isto pode ser
feito passando um vetor de duas posicoes em \code{metodo} indicando os dois metodos a serem
usados conjuntamente com o argumento \code{quebra}, um numero indicando o rendimento da curva
a partir da qual o segundo metodo deve ser utilizado.

Nestes casos, o objeto retornado tera classe \code{"interpolador"} e subclasse
\code{"interpoladorM"}, com todos os mesmos metodos disponivies.
}
\examples{

# usando dado dummy contido no pacote
interp_tri <- interpolador(colinadummy, "triangulacao")

# interpolando uma grade 20x20 no dominio da colina
pontos <- coordgrade(colinadummy, 20, 20)
grade <- predict(interp_tri, pontos)

\dontrun{
# visualizacao 3d e 2d do resultado
plot(grade)
plot(grade, "2d")
}

# INTERPOLADOR MULTIPLO
interp_mult <- interpolador(colinadummy[rend > 92], c("thinplate", "triangulacao"), 95.5)

# interpolando uma grade 20x20 no dominio da colina
pontos <- coordgrade(colinadummy, 20, 20)
grade <- predict(interp_mult, pontos)

\dontrun{
# visualizacao 3d e 2d do resultado
plot(grade)
plot(grade, "2d")
}

}
\seealso{
Other interpolador: 
\code{\link{plot.interpolador}()},
\code{\link{predict.interpolador}()}
}
\concept{interpolador}
