package challah
package utils


trait Print(value: String) {
  override def toString = value
}


extension [L, R](eithers: List[Either[L, R]])
  def squished: Either[L, List[R]] =
    eithers.iterator.squished

extension [L, R](eithers: Iterator[Either[L, R]])
  def squished: Either[L, List[R]] =
    eithers.foldLeft[Either[L, List[R]]](Right(List())) {
      (acc, x) =>
        acc.flatMap(xs => x.map(xs :+ _))
    }
