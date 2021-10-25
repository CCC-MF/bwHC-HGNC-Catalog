package de.bwhc.hgnc.test


import scala.io.Source
import scala.util.Try

import de.bwhc.catalogs.hgnc.HGNCGene
import de.bwhc.catalogs.hgnc.impl.HGNCGeneListProvider


class TestHGNCGeneListProvider extends HGNCGeneListProvider
{

  private val separator = "\t"


  private val lines =
    Source.fromResource("hgnc_complete_set.txt")
      .getLines
      .toList

  private def toSymbolList(csv: String): List[String] =
    csv.split(",")
       .map(_.trim)
       .filterNot(_.isEmpty)
       .toList

  private val header =
    lines.head.split(separator).toList

  private val hgncId       = header.indexOf("hgnc_id")
  private val symbol       = header.indexOf("symbol")
  private val name         = header.indexOf("name")
  private val prevSymbols  = header.indexOf("prev_symbol")
  private val aliasSymbols = header.indexOf("alias_symbol")


  private val genes =
    lines.tail
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


  override def geneList: Iterable[HGNCGene] =
    genes

}
