package de.bwhc.hgnc.test



import java.nio.file.Files.createTempDirectory
import scala.util.Success

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._

import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene,EnsemblId,HGNCId}


class Tests extends AnyFlatSpec
{


  val hgncTry = HGNCCatalog.getInstance


  "HGNCCatalog" should "have been successfully loaded" in {

     assert(hgncTry.isSuccess)

  }


  lazy val hgnc = hgncTry.get


  it should "contain matches for symbol 'TP53'" in {

    assert(!hgnc.genesMatchingSymbol("TP53").isEmpty)
    
  }

  it should "contain one match with HGNC-ID 'HGNC:11998'" in {

    assert(hgnc.genesMatchingSymbol("TP53").exists(_.hgncId == HGNCId("HGNC:11998")))

  }

  it should "find one gene for approved symbol 'TP53' with HGNC-ID 'HGNC:11998'" in {

    assert(hgnc.geneWithApprovedSymbol("TP53").exists(_.hgncId == HGNCId("HGNC:11998")))

  }

  it should "find one gene for approved symbol 'tp53' (lower case) with HGNC-ID 'HGNC:11998'" in {

    assert(hgnc.geneWithApprovedSymbol("tp53").exists(_.hgncId == HGNCId("HGNC:11998")))

  }

  it should "return empty result for gene with not applicable symbol 'someuselesstestsymbol'" in {

    assert(hgnc.geneWithApprovedSymbol("someuselesstestsymbol").isEmpty)

  }

  it should "contain 5 alias symbols for Gene with HGNC-ID 'HGNC:24086'" in {

    assert(
      hgnc.gene(HGNCId("HGNC:24086")).get.aliasSymbols.size == 5
    )
    
  }

  "All defined Ensembl IDs" should "start with 'ENS'" in {

    assert(
      hgnc.genes
        .filter(_.ensemblId.isDefined)
        .forall(
          _.ensemblId.get.value startsWith "ENS"
        )
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

