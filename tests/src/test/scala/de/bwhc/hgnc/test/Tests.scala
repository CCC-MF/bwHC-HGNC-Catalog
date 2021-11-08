package de.bwhc.hgnc.test



import java.nio.file.Files.createTempDirectory
import scala.util.Success

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._

import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene}


class Tests extends AnyFlatSpec
{

/*
  val hgncDir = createTempDirectory("bwhc_hgnc_tests_").toFile

  hgncDir.deleteOnExit

  System.setProperty("bwhc.hgnc.dir", hgncDir.getAbsolutePath)
*/

  val hgncTry = HGNCCatalog.getInstance


  "HGNCCatalog" should "have been successfully loaded" in {

     assert(hgncTry.isSuccess)

  }


  lazy val hgnc = hgncTry.get


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

