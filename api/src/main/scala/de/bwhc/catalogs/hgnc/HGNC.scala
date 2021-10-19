package de.bwhc.catalogs.hgnc



import java.util.ServiceLoader

import scala.util.Try
import scala.concurrent.{Future,ExecutionContext}

import play.api.libs.json.Json


/*
case class HGNCGene
(
  symbol: HGNCGene.Symbol,
  name: Option[String],
)
*/

case class HGNCGene
(
  approvedSymbol: String,
  name: String,
  previousSymbols: List[String],
  aliasSymbols: List[String]
)


object HGNCGene
{
//  case class Symbol(value: String) extends AnyVal

//  implicit val formatSymbol = Json.valueFormat[Symbol]

  implicit val format = Json.format[HGNCGene]
}


trait HGNCCatalogProvider
{
  def getInstance: HGNCCatalog
}


trait HGNCCatalog
{

  def genes: Iterable[HGNCGene]

  def genesMatchingSymbol(sym: String): Iterable[HGNCGene]

  def genesMatchingName(name: String): Iterable[HGNCGene] 

  def geneWithSymbol(sym: String): Option[HGNCGene]

  def geneWithName(name: String): Option[HGNCGene]

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
