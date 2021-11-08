package de.bwhc.hgnc.test



import java.nio.file.Files.createTempDirectory
import scala.util.Success

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._

import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene}

import scala.concurrent.Future



class AsyncTests extends AsyncFlatSpec
{

  import cats.instances.future._

/*
  val hgncDir = createTempDirectory("bwhc_hgnc_async_tests_").toFile

  hgncDir.deleteOnExit

  System.setProperty("bwhc.hgnc.dir", hgncDir.getAbsolutePath)
*/

  val hgncTry = HGNCCatalog.getInstanceF[Future]


  "HGNCCatalog" should "have been successfully loaded" in {

     assert(hgncTry.isSuccess)

  }


  lazy val hgnc = hgncTry.get


  it should "contain matches for symbol 'TP53'" in {

    for {
      genes <- hgnc.genesMatchingSymbol("TP53")
    } yield genes must not be empty
    
  }


  it should "contain 5 alias symbols for Gene with ID 'HGNC:24086'" in {

    for {
      gene <- hgnc.gene(HGNCGene.Id("HGNC:24086"))
    } yield gene.get.aliasSymbols.size must equal (5)
    
  }


  "Some previous symbols" should "exist" in {

    for {
      all   <- hgnc.genes
      genes = all.filterNot(_.previousSymbols.isEmpty).take(100)
    } yield genes must not be empty
    
  }


  "Some alias symbols" should "exist" in {

    for {
      all   <- hgnc.genes
      genes = all.filterNot(_.aliasSymbols.isEmpty).take(100)
    } yield genes must not be empty
    
  }


  "The same genes" should "have been returned queried by previous symbol (if present)" in {

    for {
      genes    <- hgnc.genes.map(_.filterNot(_.previousSymbols.isEmpty).take(100))
      synonyms <- Future.sequence(genes.map(gene => hgnc.geneWithSymbol(gene.previousSymbols.head).map(_ contains gene)))
    } yield synonyms.forall(_ == true) mustBe true
    
  }


  "The same genes" should "have been returned queried by alias symbol (if present)" in {

    for {
      genes    <- hgnc.genes.map(_.filterNot(_.aliasSymbols.isEmpty).take(100))
      synonyms <- Future.sequence(genes.map(gene => hgnc.geneWithSymbol(gene.aliasSymbols.head).map(_ contains gene)))
    } yield synonyms.forall(_ == true) mustBe true
    
  }


}
