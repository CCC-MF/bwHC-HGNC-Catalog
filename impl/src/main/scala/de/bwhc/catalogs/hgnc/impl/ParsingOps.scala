package de.bwhc.catalogs.hgnc.impl


import java.io.InputStream
import scala.io.Source
import scala.util.{Try,Using,Failure,Success}

import de.bwhc.catalogs.hgnc.{HGNCGene,EnsemblId,HGNCId}


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


