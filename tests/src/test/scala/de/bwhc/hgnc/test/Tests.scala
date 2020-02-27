package de.bwhc.hgnc.test


import scala.util.Success

import org.scalatest.AsyncFlatSpec

import de.bwhc.catalogs.hgnc._


class Tests extends AsyncFlatSpec
{

  "HGNCCatalog" should "be loaded and contain matches for symbol 'TP53'" in {

     val hgnc = HGNCCatalog.getInstance.get
    
     hgnc.genesMatchingSymbol("TP53")
         .andThen {
           case Success(gs) => gs.foreach(println)
         }
         .map(gs => assert(!gs.isEmpty))

  }


}
