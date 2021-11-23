package de.bwhc.hgnc.test


import scala.io.Source
import scala.util.Try

import de.bwhc.catalogs.hgnc.impl.{HGNCGeneLoader,JsonParsingOps,TsvParsingOps}


//class TestHGNCGeneLoader extends HGNCGeneLoader with TsvParsingOps
class TestHGNCGeneLoader extends HGNCGeneLoader with JsonParsingOps
{

  override val geneList =
    readGenes( 
      this.getClass
        .getClassLoader
        .getResourceAsStream(filename)
    )

}
