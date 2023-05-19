package de.bwhc.catalogs.hgnc



import java.util.ServiceLoader

import scala.util.Try

import play.api.libs.json.Json

import cats.{Applicative,Id}


case class HGNCGene
(
  hgncId: HGNCId,
  ensemblId: Option[EnsemblId],
  symbol: String,
  name: String,
  previousSymbols: List[String],
  aliasSymbols: List[String]
)


case class EnsemblId(value: String) extends AnyVal
object EnsemblId
{
  implicit val format = Json.valueFormat[EnsemblId]
}


case class HGNCId(value: String) extends AnyVal
object HGNCId
{
  implicit val format = Json.valueFormat[HGNCId]
}

object HGNCGene
{

  implicit val format = Json.format[HGNCGene]
}




trait HGNCCatalogProvider
{
  def getInstanceF[F[_]]: HGNCCatalog[F]
}


trait HGNCCatalog[F[_]]
{
  self =>

  import cats.syntax.functor._


  def genes(implicit F: Applicative[F]): F[Iterable[HGNCGene]]


  def gene(id: HGNCId)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    self.genes
      .map(_.find(_.hgncId == id))


  def geneWithEnsemblId(id: String)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    self.genes
      .map(_.find(_.ensemblId.exists(_.value == id)))


  // INFO: Returns List[HGNCGene] because 'sym' may be an ambiguous 'previous/alias symbol',
  // resulting in possibly more than one hit
  def geneWithSymbol(sym: String)(implicit F: Applicative[F]): F[List[HGNCGene]] =
    self.genes
      .map(
        _.filter(
          gene =>
            (gene.symbol equalsIgnoreCase sym) ||
              gene.previousSymbols.exists(_ equalsIgnoreCase sym) ||
                gene.aliasSymbols.exists(_ equalsIgnoreCase sym)
        )
        .toList
      )

  /**
   * Returns one [[Option]] of [[HGNCGene]] for 'approved' symbol ignoring ambiguous 'previous/alias symbol'
   * @param sym The symbol name
   * @return The HGNCGene or empty Option
   */
  def geneWithApprovedSymbol(sym: String)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    self.genes
      .map(
        _.find(_.symbol.equalsIgnoreCase(sym))
      )

  def geneWithName(name: String)(implicit F: Applicative[F]): F[Option[HGNCGene]] =
    self.genes
      .map(_.find(_.name.equalsIgnoreCase(name)))


  def genesMatchingSymbol(sym: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]] = {

    val lcSym = sym.toLowerCase

    self.genes
      .map(
        _.filter {
          gene =>

            (gene.symbol.toLowerCase contains lcSym) ||
              gene.previousSymbols.exists(_.toLowerCase contains lcSym) ||
                gene.aliasSymbols.exists(_.toLowerCase contains lcSym)

        }
      )
  }

  def genesMatchingName(pttrn: String)(implicit F: Applicative[F]): F[Iterable[HGNCGene]] =
    self.genes
      .map(_.filter(_.name.toLowerCase.contains(pttrn.toLowerCase)))

}


object HGNCCatalog
{

  def getInstanceF[F[_]]: Try[HGNCCatalog[F]] =
    Try {
      ServiceLoader.load(classOf[HGNCCatalogProvider])
        .iterator
        .next
        .getInstanceF[F]
    }

  final def getInstance = getInstanceF[Id]

}

