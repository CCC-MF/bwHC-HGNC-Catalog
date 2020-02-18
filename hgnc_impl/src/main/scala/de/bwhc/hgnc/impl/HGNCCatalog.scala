package de.bwhc.hgnc.impl



import de.bwhc.hgnc.api._


import scala.concurrent.Future
import scala.io.Source



class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstance: HGNCCatalog = {
    HGNCCatalogImpl
  }

}


object HGNCCatalogImpl extends HGNCCatalog
{

  import scala.concurrent.ExecutionContext.Implicits._


  private val geneList: List[HGNCGene] =
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
          HGNCGene.Name(sn(1))
        )
    )
    .toList


  def genes: Iterable[HGNCGene] = geneList


  def genesMatchingSymbol(
    sym: String
  ): Future[Iterable[HGNCGene]] =
    Future {
      geneList.filter(_.symbol.value.contains(sym))
    }

  def genesMatchingName(
    name: String
  ): Future[Iterable[HGNCGene]] =
    Future {
      geneList.filter(_.name.value.contains(name))
    }


  def geneWithSymbol(
    sym: HGNCGene.Symbol
  ): Future[Option[HGNCGene]] =
    Future {
      geneList.find(_.symbol == sym)
    }


  def geneWithName(
    name: HGNCGene.Name
  ): Future[Option[HGNCGene]] =
    Future {
      geneList.find(_.name == name)
    }

}
