package de.bwhc.catalogs.hgnc.impl



import de.bwhc.catalogs.hgnc._


import scala.concurrent.{ExecutionContext,Future}
import scala.io.Source
import scala.util.Try
import scala.util.Using

//import org.slf4j.{Logger,LoggerFactory}


class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstance: HGNCCatalog = {
    HGNCCatalogImpl
  }

}



object HGNCCatalogImpl extends HGNCCatalog
{

//  private val log = LoggerFactory.getLogger(this.getClass)

/*
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
          Try(sn(2)).map(csv => csv.split(",").map(_.trim).toList).getOrElse(List.empty).filterNot(_.isEmpty),
          Try(sn(3)).map(csv => csv.split(",").map(_.trim).toList).getOrElse(List.empty).filterNot(_.isEmpty),
        )
    )
    .toList
*/

  private val hgncUrl = "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/hgnc_complete_set.txt"


  private val separator = "\t"

  private val lines =
//    Source.fromURL(hgncUrl).getLines
    Source.fromInputStream(
      this.getClass
        .getClassLoader
        .getResourceAsStream("hgnc_complete_set.txt")
    )
    .getLines


  private val header =
    lines.next()
      .split(separator)
      .toList


  private val hgncId       = header.indexOf("hgnc_id")
  private val symbol       = header.indexOf("symbol")
  private val name         = header.indexOf("name")
  private val prevSymbols  = header.indexOf("prev_symbol")
  private val aliasSymbols = header.indexOf("alias_symbol")


  private def toSymbolList(csv: String): List[String] =
    csv.split(",")
       .map(_.trim)
       .filterNot(_.isEmpty)
       .toList 


  private val geneList: List[HGNCGene] =
    lines
      .map(_.split(separator))
      .map(
        line =>
          HGNCGene(
            HGNCGene.Id(line(hgncId)),
            line(symbol),
            line(name),
            Try(line(prevSymbols)).map(toSymbolList).getOrElse(List.empty),
            Try(line(aliasSymbols)).map(toSymbolList).getOrElse(List.empty)
          )
      )
      .toList



  override def genes = geneList


  override def gene(id: HGNCGene.Id): Option[HGNCGene] =
    geneList.find(_.id == id)


  override def geneWithSymbol(sym: String): List[HGNCGene] =
    geneList.filter(
      gene =>
        gene.symbol.equalsIgnoreCase(sym) ||
          gene.previousSymbols.exists(_.equalsIgnoreCase(sym)) ||
            gene.aliasSymbols.exists(_.equalsIgnoreCase(sym))
    )


  override def geneWithName(name: String): Option[HGNCGene] =
    geneList.find(_.name.equalsIgnoreCase(name))


  override def genesMatchingSymbol(sym: String): Iterable[HGNCGene] =
    geneList.filter {
      gene =>

        val lcSym = sym.toLowerCase

        gene.symbol.toLowerCase.contains(lcSym) ||
          gene.previousSymbols.exists(_.toLowerCase.contains(lcSym)) ||
            gene.aliasSymbols.exists(_.toLowerCase.contains(lcSym))
          
    }


  override def genesMatchingName(pttrn: String): Iterable[HGNCGene] =
    geneList.filter(_.name.toLowerCase.contains(pttrn.toLowerCase))
    

}
