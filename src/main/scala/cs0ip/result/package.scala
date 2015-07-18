package cs0ip

package object result {
  def exit[A](f: Exit[A] => A): A = Exit[A](f)

  def exitAs[A](mark: A)(f: Exit[A] => A): A = exit(f)

  def exitRes[A](f: Exit[Res[A]] => Res[A]): Res[A] = exit(f)

  def exitResAs[A](mark: A)(f: Exit[Res[A]] => Res[A]): Res[A] = exitRes(f)
}
