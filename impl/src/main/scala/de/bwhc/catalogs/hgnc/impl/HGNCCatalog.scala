package de.bwhc.catalogs.hgnc.impl



import de.bwhc.catalogs.hgnc._


import scala.concurrent.{ExecutionContext,Future}
import scala.io.Source



class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstance: HGNCCatalog = {
    HGNCCatalogImpl
  }

}


object HGNCCatalogImpl extends HGNCCatalog
{

  private lazy val geneList: Iterable[HGNCGene] =
    Source.fromInputStream(
      this.getClass
       .getClassLoader
       .getResourceAsStream("hgnc.tsv")
    )
    .getLines
    .drop(1)  // Drop the TSV file header
    .map(_.split("\t"))
    .map(
      sn =>
        HGNCGene(
          HGNCGene.Symbol(sn(0)),
          Some(sn(1))
        )
    )
    .toList


  def genes = geneList


  def genesMatchingSymbol(
    sym: String
  ): Iterable[HGNCGene] =
    geneList.filter(_.symbol.value.contains(sym))


  def genesMatchingName(
    name: String
  ): Iterable[HGNCGene] =
    geneList.filter(_.name.exists(_.contains(name)))
    

  def geneWithSymbol(
    sym: HGNCGene.Symbol
  ): Option[HGNCGene] =
    geneList.find(_.symbol == sym)


  def geneWithName(
    name: String
  ): Option[HGNCGene] =
    geneList.find(_.name.exists(_ == name))


/*  
  def genes(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]] = 
    Future.successful(geneList)


  def genesMatchingSymbol(
    sym: String
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]] =
    Future.successful(
      geneList.filter(_.symbol.value.contains(sym))
    )

  def genesMatchingName(
    name: String
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[HGNCGene]] =
    Future.successful(
      geneList.filter(_.name.exists(_.contains(name)))
    )


  def geneWithSymbol(
    sym: HGNCGene.Symbol
  )(
    implicit ec: ExecutionContext
  ): Future[Option[HGNCGene]] =
    Future.successful(
      geneList.find(_.symbol == sym)
    )


  def geneWithName(
    name: String
  )(
    implicit ec: ExecutionContext
  ): Future[Option[HGNCGene]] =
    Future.successful(
      geneList.find(_.name.exists(_ == name))
    )
*/

}
