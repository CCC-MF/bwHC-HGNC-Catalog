package de.bwhc.catalogs.hgnc



import java.util.ServiceLoader

import scala.util.Try
import scala.concurrent.{Future,ExecutionContext}



object HGNCGene
{
  case class Symbol(value: String)
}

case class HGNCGene
(
  symbol: HGNCGene.Symbol,
  name: Option[String]
)


trait HGNCCatalogProvider
{
  def getInstance: HGNCCatalog
}


trait HGNCCatalog
{

  def genes(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]]

  def genesMatchingSymbol(
    sym: String
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]]

  def genesMatchingName(
    name: String
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]] 

  def geneWithSymbol(
    sym: HGNCGene.Symbol
  )(
    implicit ec: ExecutionContext
  ): Future[Option[HGNCGene]]

  def geneWithName(
    name: String
  )(
    implicit ec: ExecutionContext
  ): Future[Option[HGNCGene]]

}


object HGNCCatalog
{

  def getInstance: Try[HGNCCatalog] = Try {
    ServiceLoader.load(classOf[HGNCCatalogProvider])
      .iterator
      .next
      .getInstance
  }

}
