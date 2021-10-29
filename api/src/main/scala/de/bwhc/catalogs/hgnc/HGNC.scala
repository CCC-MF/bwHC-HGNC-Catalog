package de.bwhc.catalogs.hgnc



import java.util.ServiceLoader

import scala.util.Try

import play.api.libs.json.Json

import cats.{Applicative,Id}


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
  def getInstanceF[F[_]]: HGNCCatalog[F]
}


trait HGNCCatalog[F[_]]
{

  def genes(implicit F: Applicative[F]): F[Iterable[HGNCGene]]


  def gene(id: HGNCGene.Id)(implicit F: Applicative[F]): F[Option[HGNCGene]]

  // INFO: Returns List[HGNCGene] because 'sym' may be an ambiguous 'previous/alias symbol',
  // resulting in possibly more than one hit
  def geneWithSymbol(sym: String)(implicit F: Applicative[F]): F[List[HGNCGene]]

  def geneWithName(name: String)(implicit F: Applicative[F]): F[Option[HGNCGene]]

  def genesMatchingSymbol(sym: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]]

  def genesMatchingName(name: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]]

}


object HGNCCatalog
{

  def getInstanceF[F[_]]: Try[HGNCCatalog[F]] =
    Try {
      ServiceLoader.load(classOf[HGNCCatalogProvider])
        .iterator
        .next
        .getInstanceF[F]
    }

  final def getInstance = getInstanceF[Id]

}


/*
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

  def getInstance: Try[HGNCCatalog] =
    Try {
      ServiceLoader.load(classOf[HGNCCatalogProvider])
        .iterator
        .next
        .getInstance
    }

}
*/
