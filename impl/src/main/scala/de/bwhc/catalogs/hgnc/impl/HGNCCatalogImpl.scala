package de.bwhc.catalogs.hgnc.impl


import java.io.{File,FileInputStream,InputStream}
import java.nio.file.{Files,StandardCopyOption}
import java.nio.file.attribute.BasicFileAttributes
import java.net.URL
import java.util.ServiceLoader
import java.time.{Instant,Duration}
import java.time.temporal.ChronoUnit.DAYS

import scala.concurrent.{ExecutionContext,Future}
import scala.io.Source
import scala.util.{Try,Using,Failure,Success}

import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalogProvider,HGNCCatalog}

import cats.Applicative
import cats.syntax.functor._

import org.slf4j.{Logger,LoggerFactory}


class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstanceF[F[_]]: HGNCCatalog[F] = {
    new HGNCCatalogImpl[F]
  }

}


class HGNCCatalogImpl[F[_]] extends HGNCCatalog[F]
{

  override def genes(implicit F: Applicative[F]) =
    F.pure(HGNCCatalogImpl.geneLoader.geneList)


  override def gene(id: HGNCGene.Id)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    genes.map(_.find(_.id == id))


  override def geneWithSymbol(sym: String)(implicit F: Applicative[F]): F[List[HGNCGene]] =
    genes.map(
      _.filter(
        gene =>
          gene.symbol.equalsIgnoreCase(sym) ||
            gene.previousSymbols.exists(_.equalsIgnoreCase(sym)) ||
              gene.aliasSymbols.exists(_.equalsIgnoreCase(sym))
      )
      .toList
    )

  override def geneWithName(name: String)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    genes.map(_.find(_.name.equalsIgnoreCase(name)))


  override def genesMatchingSymbol(sym: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]] =
    genes.map(
      _.filter {
        gene =>

          val lcSym = sym.toLowerCase

          gene.symbol.toLowerCase.contains(lcSym) ||
            gene.previousSymbols.exists(_.toLowerCase.contains(lcSym)) ||
              gene.aliasSymbols.exists(_.toLowerCase.contains(lcSym))
          
      }
    )

  override def genesMatchingName(pttrn: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]] =
    genes.map(_.filter(_.name.toLowerCase.contains(pttrn.toLowerCase)))
    

}


trait HGNCGeneLoader
{
  
  def geneList: Iterable[HGNCGene]

}


trait TsvHGNCGeneLoader extends HGNCGeneLoader
{
  
  protected final val separator = "\t"

  protected final def toSymbolList(csv: String): List[String] = {
    csv.replace("\"","")
       .split("\\|")
       .map(_.trim)
       .filterNot(_.isEmpty)
       .toList
  }
  
}

/*
trait JsonHGNCGeneLoader extends HGNCGeneLoader
{

  import play.api.libs.json.{Json,JsObject}

  protected final def readJson(in: InputStream) = {

    val json = Json.parse(in)

    (json \ "response" \ "docs").as[Iterable[JsObject]]
      .map(
        obj =>
          HGNCGene(
            HGNCGene.Id((obj \ "hgnc_id").as[String]),
            (obj \ "symbol").as[String],
            (obj \ "name").as[String],
            (obj \ "prev_symbol").asOpt[List[String]].getOrElse(List.empty),
            (obj \ "alias_symbol").asOpt[List[String]].getOrElse(List.empty)
          )
      )
  }

}  
*/

private object HGNCCatalogImpl
{

  private val log = LoggerFactory.getLogger(this.getClass)

/*
  private class DefaultHGNCGeneLoader extends JsonHGNCGeneLoader
  {

    private val sysProperty = "bwhc.hgnc.dir"

    private val hgncUrl =
      "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/json/hgnc_complete_set.json"


    private val hgncFile = 
      Option(System.getProperty(sysProperty))
        .map(new File(_))
        .map {
          dir =>
            dir.mkdirs
            dir
        }  
        .map(new File(_,"hgnc_complete_set.json"))


    private def loadGenes: Iterable[HGNCGene] = {
    
      Try(hgncFile.get)
        .flatMap {
          file =>
            if (
              file.createNewFile ||                                          // Check if file had to be created because it didn't exist yet
              Files.readAttributes(file.toPath,classOf[BasicFileAttributes]) // or whether its last update is older than 7 days
                .lastModifiedTime
                .toInstant
                .isBefore(Instant.now.minus(Duration.of(7,DAYS)))
            ){
              log.info(s"Fetching current complete HGNC set from $hgncUrl")
              
              val connection = new URL(hgncUrl).openConnection

              connection.setReadTimeout(3000) // time-out in milli-seconds

              Using(connection.getInputStream)(
                in => 
                  Files.copy(in,file.toPath,StandardCopyOption.REPLACE_EXISTING) 
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
        }
        .map(
          new FileInputStream(_)
        )
        .recover {
          case n: NoSuchElementException =>
            log.warn(s"This error occurs most likely due to undefined JVM property '$sysProperty'")
            log.warn("Falling back to pre-packaged HGNC set")

            this.getClass.getClassLoader.getResourceAsStream("hgnc_complete_set.json")

          case t =>
            log.warn(s"Failed to get current HGNC set from $hgncUrl", t)
            log.warn("Falling back to pre-packaged HGNC set")

            this.getClass.getClassLoader.getResourceAsStream("hgnc_complete_set.json")
        }
        .map(readJson)
        .get

    }

    private var loadedGenes =
      loadGenes

    private var lastUpdate = Instant.now


    override def geneList: Iterable[HGNCGene] = {

      if (lastUpdate.isBefore(Instant.now.minus(Duration.of(7,DAYS)))){
        log.info("Updating cached HGNC Gene set")
        loadedGenes = loadGenes
      }

      loadedGenes
    }

  }
*/

  private class DefaultHGNCGeneLoader extends TsvHGNCGeneLoader
  {

    private val sysProperty = "bwhc.hgnc.dir"

    private val hgncUrl =
      "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/hgnc_complete_set.txt"


    private val hgncFile = 
      Option(System.getProperty(sysProperty))
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
                
                val connection = new URL(hgncUrl).openConnection

                connection.setReadTimeout(3000) // time-out in milli-seconds

                Using(connection.getInputStream)(
                  in => 
                    Files.copy(in,file.toPath,StandardCopyOption.REPLACE_EXISTING) 
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
          case n: NoSuchElementException =>
            log.warn(s"Failed to import HGNC gene set. This error occurs most likely due to undefined JVM property '$sysProperty'")
            log.warn("Falling back to pre-packaged HGNC set")

            Source.fromResource("hgnc_complete_set.txt")

          case t =>
            log.warn(s"Failed to get current HGNC set from $hgncUrl", t)
            log.warn("Falling back to pre-packaged HGNC set")

            Source.fromResource("hgnc_complete_set.txt")
        }
        .map(_.getLines)
        .get
        .toList

    }

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


  val geneLoader: HGNCGeneLoader =
    Try(
      ServiceLoader.load(classOf[HGNCGeneLoader])
        .iterator
        .next
    )
    .getOrElse(new DefaultHGNCGeneLoader)

}



/*
class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  def getInstance: HGNCCatalog = {
    HGNCCatalogImpl
  }

}

object HGNCCatalogImpl extends HGNCCatalog
{

  private val log = LoggerFactory.getLogger(this.getClass)


  private class DefaultHGNCGeneLoader extends HGNCGeneLoader
  {

    private val sysProperty = "bwhc.hgnc.dir"

    private val hgncUrl =
      "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/hgnc_complete_set.txt"


    private val hgncFile = 
      Option(System.getProperty(sysProperty))
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
                
                val connection = new URL(hgncUrl).openConnection

                connection.setReadTimeout(3000) // time-out in milli-seconds

                Using(connection.getInputStream)(
                  in => 
                    Files.copy(in,file.toPath,StandardCopyOption.REPLACE_EXISTING) 
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
          case n: NoSuchElementException =>
            log.warn(s"This error occurs most likely due to undefined JVM property '$sysProperty'")
            log.warn("Falling back to pre-packaged HGNC set")

            Source.fromResource("hgnc_complete_set.txt")

          case t =>
            log.warn(s"Failed to get current HGNC set from $hgncUrl", t)
            log.warn("Falling back to pre-packaged HGNC set")

            Source.fromResource("hgnc_complete_set.txt")
        }
        .map(_.getLines)
        .get
        .toList

    }

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


//  private val geneLoader: HGNCGeneLoader =
  val geneLoader: HGNCGeneLoader =
    Try(
      ServiceLoader.load(classOf[HGNCGeneLoader])
        .iterator
        .next
    )
    .getOrElse(new DefaultHGNCGeneLoader)



  override def genes =
    geneLoader.geneList


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
*/
