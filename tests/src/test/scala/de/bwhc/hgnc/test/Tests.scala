package de.bwhc.hgnc.test



import java.nio.file.Files.createTempDirectory
import scala.util.Success

import org.scalatest.FlatSpec

import de.bwhc.catalogs.hgnc._


object Setup
{
/*
  val tmpDir = createTempDirectory("bwhc_hgnc_test_")

  System.setProperty("bwhc.hgnc.dir", tmpDir.toAbsolutePath.toString)

  tmpDir.toFile.deleteOnExit
*/

  lazy val hgncTry = HGNCCatalog.getInstance

}


class Tests extends FlatSpec
{

  "HGNCCatalog" should "have been successfully loaded" in {

     assert(Setup.hgncTry.isSuccess)

  }


  lazy val hgnc = Setup.hgncTry.get


  it should "contain matches for symbol 'TP53'" in {

    assert(!hgnc.genesMatchingSymbol("TP53").isEmpty)
    
  }


  it should "contain 5 alias symbols for Gene with ID 'HGNC:24086'" in {

    assert(
      hgnc.gene(HGNCGene.Id("HGNC:24086")).get.aliasSymbols.size == 5
    )
    
  }


  "Some previous symbols" should "exist" in {

    assert(
      !hgnc.genes
        .filterNot(_.previousSymbols.isEmpty)
        .take(100)
        .isEmpty
    )
    
  }


  "Some alias symbols" should "exist" in {

    assert(
      !hgnc.genes
        .filterNot(_.aliasSymbols.isEmpty)
        .take(100)
        .isEmpty
    )
    
  }


  "The same genes" should "have been returned queried by previous symbol (if present)" in {

    assert(
      hgnc.genes
        .filterNot(_.previousSymbols.isEmpty)
        .take(100)
        .forall(
          gene => hgnc.geneWithSymbol(gene.previousSymbols.head).contains(gene)
        )
    )
    
  }

  "The same genes" should "have been returned queried by alias symbol (if present)" in {

    assert(
      hgnc.genes
        .filterNot(_.aliasSymbols.isEmpty)
        .take(100)
        .forall(
          gene => hgnc.geneWithSymbol(gene.aliasSymbols.head).contains(gene)
        )

    )

  }


}
