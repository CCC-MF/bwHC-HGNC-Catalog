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

import de.bwhc.catalogs.hgnc.{HGNCGene,EnsemblId,HGNCId,HGNCCatalogProvider,HGNCCatalog}

import cats.Applicative
import cats.syntax.functor._

import org.slf4j.{Logger,LoggerFactory}


class HGNCCatalogProviderImpl extends HGNCCatalogProvider
{

  override def getInstanceF[F[_]]: HGNCCatalog[F] = {
    new HGNCCatalogImpl[F]
  }

}


class HGNCCatalogImpl[F[_]] extends HGNCCatalog[F]
{

  override def genes(implicit F: Applicative[F]) =
    F.pure(
      HGNCCatalogImpl.geneLoader.geneList
    )
}


trait HGNCGeneLoader
{
  val url: String

  val filename: String 

  def geneList: Iterable[HGNCGene]
}


trait TsvParsingOps
{
  this: HGNCGeneLoader =>
 
 
  override val filename = 
    "hgnc_complete_set.txt"

  override val url =
    s"https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/$filename"


  final val separator = "\t"

  final def toSymbolList(csv: String): List[String] =
    csv.replace("\"","")
       .split("\\|")
       .map(_.trim)
       .filterNot(_.isEmpty)
       .toList
  
  final def readGenes(in: InputStream): List[HGNCGene] = {

    import scala.util.chaining._

    val lines =
      Source.fromInputStream(in)
        .getLines
        .toList

    val header =
      lines.head
        .split(separator)
        .toList
 
    val hgncId       = header.indexOf("hgnc_id")
    val ensemblId    = header.indexOf("ensembl_gene_id")
    val symbol       = header.indexOf("symbol")
    val name         = header.indexOf("name")
    val prevSymbols  = header.indexOf("prev_symbol")
    val aliasSymbols = header.indexOf("alias_symbol")
 
    val genes =
      lines.tail
        .map(_ split separator)
        .map {
          line =>
            HGNCGene(
              HGNCId(line(hgncId)),
              Try(EnsemblId(line(ensemblId))).toOption,
              line(symbol),
              line(name),
              line(prevSymbols) pipe toSymbolList,
              line(aliasSymbols) pipe toSymbolList
            )
        }

    in.close

    genes
  }

}


trait JsonParsingOps
{
  this: HGNCGeneLoader =>

  import play.api.libs.json.{Json,JsObject}

  override val filename = 
    "hgnc_complete_set.json"

  override val url =
    s"https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/json/$filename"


  final def readGenes(in: InputStream): List[HGNCGene] = {

    val json = Json.parse(in)

    (json \ "response" \ "docs").as[Iterable[JsObject]]
      .map(
        obj =>
          HGNCGene(
            HGNCId((obj\ "hgnc_id").as[String]),
            (obj \ "ensembl_gene_id").asOpt[String].map(EnsemblId(_)),
            (obj \ "symbol").as[String],
            (obj \ "name").as[String],
            (obj \ "prev_symbol").asOpt[List[String]].getOrElse(List.empty),
            (obj \ "alias_symbol").asOpt[List[String]].getOrElse(List.empty)
          )
      )
      .toList
  }

}  


private object HGNCCatalogImpl
{

  private val log = LoggerFactory.getLogger(this.getClass)

//  private class DefaultHGNCGeneLoader extends HGNCGeneLoader with TsvParsingOps
  private class DefaultHGNCGeneLoader extends HGNCGeneLoader with JsonParsingOps
  {

    final val sysProperty = "bwhc.hgnc.dir"

    private val hgncFile = 
      Option(System.getProperty(sysProperty))
        .map(new File(_))
        .map {
          dir =>
            dir.mkdirs
            dir
        }  
        .map(new File(_,filename))


    private def loadInput: InputStream = {

      Try(hgncFile.get)
        .flatMap {
          file =>
            (
              if (
                !file.exists ||                                                // Check if file doesn't exist yet
                Files.readAttributes(file.toPath,classOf[BasicFileAttributes]) // or whether its last update is older than 7 days
                  .lastModifiedTime
                  .toInstant
                  .isBefore(Instant.now.minus(Duration.of(7,DAYS)))
              ){
                log.info(s"Fetching current complete HGNC set from $url")
                
                val connection = new URL(url).openConnection

                connection.setConnectTimeout(3000) // connection build-up timeout (in milli-seconds)
                connection.setReadTimeout(10000) // timeout in milli-seconds

                val tmpFile = Files.createTempFile(file.getParentFile.toPath,"tmp_hgnc_",".txt")

                Using(connection.getInputStream)(
                  in => Files.copy(in,tmpFile,StandardCopyOption.REPLACE_EXISTING) 
                )
                .transform(
                  s => {
                    Files.move(tmpFile,file.toPath,StandardCopyOption.REPLACE_EXISTING) 
                    log.info(s"Successfully updated HGNC catalog file")
                    Success(file)
                  },
                  t => {
                    log.error(s"Error updating HGNC catalog file; deleting tmp file",t)
                    Files.delete(tmpFile)
                    Failure(t)
                  }
                )
              } else {
                Success(file)
              }
            )
            .map(new FileInputStream(_))
      
        }
        .recover {
          case n: NoSuchElementException =>
            log.warn(s"Failed to import HGNC gene set. This error occurs most likely due to undefined JVM property '$sysProperty'")
            log.warn("Falling back to pre-packaged HGNC set")

            this.getClass.getClassLoader.getResourceAsStream(filename)

          case t =>
            log.warn(s"Failed to get current HGNC set from $url", t)
            log.warn("Falling back to pre-packaged HGNC set")

            this.getClass.getClassLoader.getResourceAsStream(filename)
        }
        .get

    }


    private var loadedGenes =
      readGenes(loadInput)

    private var lastUpdate = Instant.now


    override def geneList: Iterable[HGNCGene] = {

      if (lastUpdate isBefore Instant.now.minus(Duration.of(7,DAYS))){
        log.info("Updating cached HGNC Gene set")
        loadedGenes = readGenes(loadInput)
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
