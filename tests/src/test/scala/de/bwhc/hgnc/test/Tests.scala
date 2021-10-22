package de.bwhc.hgnc.test


import scala.util.Success

import org.scalatest.FlatSpec

import de.bwhc.catalogs.hgnc._


object Util
{

  val hgncTry = HGNCCatalog.getInstance

}


class Tests extends FlatSpec
{

  "HGNCCatalog" should "have been successfully loaded" in {

     assert(Util.hgncTry.isSuccess)

  }


  lazy val hgnc = Util.hgncTry.get


  it should "contain matches for symbol 'TP53'" in {

    assert(!hgnc.genesMatchingSymbol("TP53").isEmpty)
    
  }


  it should "return the same genes as queried by previous symbol (if present)" in {

    assert(
      hgnc.genes
        .filterNot(_.previousSymbols.isEmpty)
        .take(100)
        .forall(
          gene => hgnc.geneWithSymbol(gene.previousSymbols.head).contains(gene)
        )
    )
    
  }

  it should "return the same genes as queried by alias symbol (if present)" in {

    val passed =
      hgnc.genes
        .filterNot(_.aliasSymbols.isEmpty)
        .take(100)
        .forall(
          gene => hgnc.geneWithSymbol(gene.aliasSymbols.head).contains(gene)
        )

    assert(passed)
    
  }


}
