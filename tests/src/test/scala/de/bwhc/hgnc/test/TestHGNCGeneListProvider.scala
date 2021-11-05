package de.bwhc.hgnc.test


import scala.io.Source
import scala.util.Try

import de.bwhc.catalogs.hgnc.impl.{HGNCGeneLoader,TsvParsingOps}


class TestHGNCGeneLoader extends HGNCGeneLoader with TsvParsingOps
{

  override val geneList =
    readGenes( 
      this.getClass
        .getClassLoader
        .getResourceAsStream(filename)
    )

}
