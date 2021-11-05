package de.bwhc.hgnc.test


import scala.io.Source
import scala.util.Try

import de.bwhc.catalogs.hgnc.impl.{HGNCGeneLoader,TsvParsingOps}
//import de.bwhc.catalogs.hgnc.impl.JsonHGNCGeneLoader


class TestHGNCGeneLoader extends HGNCGeneLoader with TsvParsingOps
{

  override val geneList =
    readGenes( 
      this.getClass
        .getClassLoader
        .getResourceAsStream("hgnc_complete_set.txt")
    )

}

/*
class TestHGNCGeneLoader extends JsonHGNCGeneLoader
{

  override val geneList: Iterable[HGNCGene] =
    this.readJson(this.getClass.getClassLoader.getResourceAsStream("hgnc_complete_set.json"))

}
*/

