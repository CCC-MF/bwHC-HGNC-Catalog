package de.bwhc.catalogs.hgnc.impl



import de.bwhc.catalogs.hgnc._


import scala.concurrent.{ExecutionContext,Future}
import scala.io.Source
import scala.util.Try


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
          sn(0),
          sn(1),
          Try(sn(2)).map(_.split(",").map(_.trim).toList).getOrElse(List.empty),
          Try(sn(3)).map(_.split(",").map(_.trim).toList).getOrElse(List.empty),
        )
    )
    .toList


  def genes = geneList


  def genesMatchingSymbol(sym: String): Iterable[HGNCGene] =
    geneList.filter {
      gene =>

        val lcSym = sym.toLowerCase

        gene.approvedSymbol.toLowerCase.contains(lcSym) ||
        gene.previousSymbols.exists(_.toLowerCase.contains(lcSym)) ||
        gene.aliasSymbols.exists(_.toLowerCase.contains(lcSym))
          
    }


  def genesMatchingName(pttrn: String): Iterable[HGNCGene] =
    geneList.filter(_.name.toLowerCase.contains(pttrn.toLowerCase))
    

  def geneWithSymbol(sym: String): Option[HGNCGene] =
    geneList.find(
      gene =>
        gene.approvedSymbol.equalsIgnoreCase(sym) ||
        gene.previousSymbols.exists(_.equalsIgnoreCase(sym)) ||
        gene.aliasSymbols.exists(_.equalsIgnoreCase(sym))
    )


  def geneWithName(name: String): Option[HGNCGene] =
    geneList.find(_.name.equalsIgnoreCase(name))


}
