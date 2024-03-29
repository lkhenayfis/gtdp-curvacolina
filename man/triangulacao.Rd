% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod-triangulacao.r
\name{triangulacao}
\alias{triangulacao}
\title{Triangulacao De \code{curvacolina}}
\usage{
triangulacao(colina, tessfunc = tessdelaunay, modo = "pot", ...)
}
\arguments{
\item{colina}{objeto \code{curvacolina} retornado pelas funcoes de leitura}

\item{tessfunc}{funcao ou string com nome da funcao pela qual executar a tesselacao do espaco.
Ver Detalhes}

\item{modo}{um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
modelada}

\item{...}{nao possui funcao, so existe para compatibilizacao com a chamada generica de
\code{\link{interpolador}}}
}
\value{
objeto da classe \code{triangulacao} contendo a tesselacao da curva colina
}
\description{
Funcao interna executada quando \code{metodo = "triangulacao"} em \code{interpolador}. O
argumento \code{tessfunc} permite controlar como o espaco projetado sera tesselado em triangulos.
Atualmente ha duas opcoes implementadas no pacote:
}
\details{
\itemize{
\item{\code{\link{tessdelaunay}}}
\item{\code{\link{tessradial}}}
}

A primeira executa a tesselacao pelo metodo de Delaunay atraves da funcao
\code{\link[geometry]{delaunayn}}. A segunda e uma versao alternativa quase igual a primeira, com
excessao dos triangulos definidos entre a ultima curva e o maximo. Pelo metodo de Delaunay e
possivel que acontecam platos, isto e, triangulos cujos tres vertices pertencem a mesma curva.
\code{tessradial} interfere apenas nessa regiao, forcando para que todos os triangulos entre a
ultima curva e o maximo tenham este ponto como um vertice, deixando os dois restantes como pontos
adjacentes na ultima curva.

Esta funcao nao deve ser chamada pelo usuario diretamente na maioria dos casos
}
