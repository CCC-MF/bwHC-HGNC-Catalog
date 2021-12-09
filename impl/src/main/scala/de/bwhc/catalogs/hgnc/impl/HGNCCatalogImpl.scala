package de.bwhc.catalogs.hgnc.impl


import java.io.{File,FileInputStream,InputStream}
import java.nio.file.{Files,StandardCopyOption}
import java.nio.file.attribute.BasicFileAttributes
import java.net.{URL,Proxy,InetSocketAddress}
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


private object HGNCCatalogImpl
{

  private val log = LoggerFactory.getLogger(this.getClass)


  private val proxy: Option[Proxy] =
    Option(System.getProperty("https.proxyHost"))
      .map(host => (host,System.getProperty("https.proxyPort")))
      .orElse(
        Option(System.getProperty("http.proxyHost"))
          .map(host => (host,System.getProperty("http.proxyPort")))
      )
      .map {
        case (host,port) =>
          new Proxy(Proxy.Type.HTTP, new InetSocketAddress(host,port.toInt))
      }


  private class ScheduledHGNCGeneLoader extends HGNCGeneLoader with JsonParsingOps
  {

    import java.util.concurrent.atomic.AtomicReference
    import java.util.concurrent.{Executors,ScheduledExecutorService}
    import java.util.concurrent.TimeUnit.SECONDS
    import java.time.{Duration,LocalTime}


    final val sysProperty = "bwhc.hgnc.dir"

    private val turnoverPeriod = Duration.of(7,DAYS)

    private val hgncFile = 
      Option(System.getProperty(sysProperty))
        .map(new File(_))
        .map {
          dir =>
            dir.mkdirs
            dir
        }  
        .map(new File(_,filename))


    private val executor =
      Executors.newSingleThreadScheduledExecutor

    executor.scheduleAtFixedRate(
      () => {
        log.info("Updating cached HGNC catalog")
        loadedGenes.set(readGenes(loadInput))
      },
      Duration.between(LocalTime.now,LocalTime.MAX).toSeconds,  // delay execution until Midnight
      3600*24,                                                  // re-load every 24h (see above for actual turn-over period)
      SECONDS
    ) 


    private def loadInput: InputStream = {

      Try(hgncFile.get)
        .flatMap {
          file =>
            (
              if (
                !file.exists ||                                                // Check if file doesn't exist yet
                Files.readAttributes(file.toPath,classOf[BasicFileAttributes]) // or whether its last update is older than 'turnoverPeriod'
                  .lastModifiedTime
                  .toInstant
                  .isBefore(Instant.now minus turnoverPeriod)
              ){

                log.info(s"Fetching current complete HGNC set from $url")
                
                val connection =
                  proxy match {
                    case Some(p) => new URL(url).openConnection(p)
                    case None    => new URL(url).openConnection
                  }

                connection.setConnectTimeout(2000) // connection build-up timeout (in milli-seconds)
                connection.setReadTimeout(5000) // timeout in milli-seconds

                val tmpFile = Files.createTempFile(file.getParentFile.toPath,s"tmp_${filename}","")

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


    private val loadedGenes =
      new AtomicReference(readGenes(loadInput))


    override def geneList: Iterable[HGNCGene] =
      loadedGenes.get

  }


  val geneLoader: HGNCGeneLoader =
    Try(
      ServiceLoader.load(classOf[HGNCGeneLoader])
        .iterator
        .next
    )
    .getOrElse(new ScheduledHGNCGeneLoader)

}



/*
private object HGNCCatalogImpl
{

  private val log = LoggerFactory.getLogger(this.getClass)


  private val proxy: Option[Proxy] =
    Option(System.getProperty("https.proxyHost"))
      .map(host => (host,System.getProperty("https.proxyPort")))
      .orElse(
        Option(System.getProperty("http.proxyHost"))
          .map(host => (host,System.getProperty("http.proxyPort")))
      )
      .map {
        case (host,port) =>
          new Proxy(Proxy.Type.HTTP, new InetSocketAddress(host,port.toInt))
      }


//  private class ScheduledHGNCGeneLoader extends HGNCGeneLoader with TsvParsingOps
  private class ScheduledHGNCGeneLoader extends HGNCGeneLoader with JsonParsingOps
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
                
                val connection =
                  proxy match {
                    case Some(p) => new URL(url).openConnection(p)
                    case None    => new URL(url).openConnection
                  }

                connection.setConnectTimeout(2000) // connection build-up timeout (in milli-seconds)
                connection.setReadTimeout(5000) // timeout in milli-seconds

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


    override def geneList: Iterable[HGNCGene] =
      this.synchronized {

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
    .getOrElse(new ScheduledHGNCGeneLoader)

}
*/
