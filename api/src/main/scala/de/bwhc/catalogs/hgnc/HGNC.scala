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
  id: HGNCGene.Id,
  symbol: String,
  name: String,
  previousSymbols: List[String],
  aliasSymbols: List[String]
)


object HGNCGene
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[HGNCGene]
}



trait HGNCCatalogProvider
{
  def getInstance: HGNCCatalog
}


trait HGNCCatalog
{

  def genes: Iterable[HGNCGene]


  def gene(id: HGNCGene.Id): Option[HGNCGene]

  // INFO: Returns List[HGNCGene] because 'sym' may be an ambiguous 'previous/alias symbol',
  // resulting in possibly more than one hit
  def geneWithSymbol(sym: String): List[HGNCGene]

  def geneWithName(name: String): Option[HGNCGene]

  def genesMatchingSymbol(sym: String): Iterable[HGNCGene]

  def genesMatchingName(name: String): Iterable[HGNCGene] 

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
