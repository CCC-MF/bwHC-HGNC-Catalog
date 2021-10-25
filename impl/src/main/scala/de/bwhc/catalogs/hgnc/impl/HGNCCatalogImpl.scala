package de.bwhc.catalogs.hgnc.impl


import java.io.File
import java.nio.file.{Files,StandardCopyOption}
import java.nio.file.attribute.BasicFileAttributes
import java.net.URL
import java.util.ServiceLoader
import java.time.{Instant,Duration}
import java.time.temporal.ChronoUnit.DAYS

import scala.concurrent.{ExecutionContext,Future}
import scala.io.Source
import scala.util.{Try,Using,Failure,Success}

import de.bwhc.catalogs.hgnc._

import org.slf4j.{Logger,LoggerFactory}


class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstance: HGNCCatalog = {
    HGNCCatalogImpl
  }

}


trait HGNCGeneListProvider
{
  def geneList: Iterable[HGNCGene]
}


object HGNCCatalogImpl extends HGNCCatalog
{

  private val log = LoggerFactory.getLogger(this.getClass)


  private class DefaultHGNCGeneListProvider extends HGNCGeneListProvider
  {

    private val hgncUrl =
      "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/hgnc_complete_set.txt"

    private val separator = "\t"


    private val hgncFile = 
      Option(System.getProperty("bwhc.hgnc.dir"))
        .map(new File(_))
        .map {
          dir =>
            dir.mkdirs
            dir
        }  
        .map(new File(_,"hgnc_complete_set.txt"))


    private def loadLines: List[String] = {

      Try(hgncFile.get)
        .flatMap {
          file =>
            (
              if (
                file.createNewFile ||                                          // Check if file had to be created because it didn't exist yet
                Files.readAttributes(file.toPath,classOf[BasicFileAttributes]) // or whether its last update is older than 7 days
                  .lastModifiedTime
                  .toInstant
                  .isBefore(Instant.now.minus(Duration.of(7,DAYS)))
              ){
                log.info(s"Fetching current complete HGNC set from $hgncUrl")
                
                Using(
                  new URL(hgncUrl).openStream
                )(
                  Files.copy(_,file.toPath,StandardCopyOption.REPLACE_EXISTING) 
                )
                .transform(
                  s => Success(file),
                  t => {
                    log.error(s"Error updating HGNC catalog file; deleting it",t)
                    file.delete
                    Failure(t)
                  }
                )
              } else {
                Success(file)
              }
            )
            .map(Source.fromFile(_))
      
        }
        .recover {
          case t =>
            log.warn(s"Failed to get current HGNC set from $hgncUrl", t)
            log.warn("Falling back to pre-packaged HGNC set")

            Source.fromResource("hgnc_complete_set.txt")
        }
        .map(_.getLines)
        .get
        .toList

    }

  
    private def toSymbolList(csv: String): List[String] =
      csv.split(",")
         .map(_.trim)
         .filterNot(_.isEmpty)
         .toList 
  
  
    private def toGenes(lines: List[String]): List[HGNCGene] = {

      val header =
        lines.head.split(separator).toList
 
      val hgncId       = header.indexOf("hgnc_id")
      val symbol       = header.indexOf("symbol")
      val name         = header.indexOf("name")
      val prevSymbols  = header.indexOf("prev_symbol")
      val aliasSymbols = header.indexOf("alias_symbol")
  
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

    }


    private var loadedGenes =
      toGenes(loadLines)

    private var lastUpdate = Instant.now


    override def geneList: Iterable[HGNCGene] = {

      if (lastUpdate.isBefore(Instant.now.minus(Duration.of(7,DAYS)))){
        log.info("Updating cached HGNC Gene set")
        loadedGenes = toGenes(loadLines)
      }

      loadedGenes
    }

  }


  private val geneListProvider: HGNCGeneListProvider =
    Try(
      ServiceLoader.load(classOf[HGNCGeneListProvider])
        .iterator
        .next
    )
    .getOrElse(new DefaultHGNCGeneListProvider)



  override def genes =
    geneListProvider.geneList


  override def gene(id: HGNCGene.Id): Option[HGNCGene] =
    genes.find(_.id == id)


  override def geneWithSymbol(sym: String): List[HGNCGene] =
    genes.filter(
      gene =>
        gene.symbol.equalsIgnoreCase(sym) ||
          gene.previousSymbols.exists(_.equalsIgnoreCase(sym)) ||
            gene.aliasSymbols.exists(_.equalsIgnoreCase(sym))
    )
    .toList


  override def geneWithName(name: String): Option[HGNCGene] =
    genes.find(_.name.equalsIgnoreCase(name))


  override def genesMatchingSymbol(sym: String): Iterable[HGNCGene] =
    genes.filter {
      gene =>

        val lcSym = sym.toLowerCase

        gene.symbol.toLowerCase.contains(lcSym) ||
          gene.previousSymbols.exists(_.toLowerCase.contains(lcSym)) ||
            gene.aliasSymbols.exists(_.toLowerCase.contains(lcSym))
          
    }


  override def genesMatchingName(pttrn: String): Iterable[HGNCGene] =
    genes.filter(_.name.toLowerCase.contains(pttrn.toLowerCase))
    

}
