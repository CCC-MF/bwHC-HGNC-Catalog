package de.bwhc.catalogs.hgnc



import java.util.ServiceLoader

import scala.util.Try
import scala.concurrent.Future



object HGNCGene
{
  case class Symbol(value: String)
  case class Name(value: String)
}

case class HGNCGene
(
  symbol: HGNCGene.Symbol,
  name: HGNCGene.Name
//  symbol: String,
//  name: String
)


trait HGNCCatalogProvider
{
  def getInstance: HGNCCatalog
}


trait HGNCCatalog
{

  def genes: Iterable[HGNCGene]

  def genesMatchingSymbol(sym: String): Future[Iterable[HGNCGene]]

  def genesMatchingName(name: String): Future[Iterable[HGNCGene]] 

  def geneWithSymbol(sym: HGNCGene.Symbol): Future[Option[HGNCGene]]

  def geneWithName(name: HGNCGene.Name): Future[Option[HGNCGene]]

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
