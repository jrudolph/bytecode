package net.virtualvoid.bytecode

trait BranchingInstructions {
  /*
   * Branch based on the current integer value on top of the stack. The first parameter
   * has to include all candidate key values to branch on. The second parameter defines the mapping
   * from keys to branch. Match on `Some(key)` to branch on the key, match on `None` for the default
   * branch.
   *
   * Example:
   *   lookupSwitch(1, 5)(f => {
   *     case Some(1) => f ~ ldc("eins")
   *     case Some(5) => f ~ ldc("fuenf")
   *     case None => f ~ ldc("unbekannte Zahl")
   *   })
   */
  def lookupSwitch[R <: Stack, ST2 <: Stack](candidates: Int*)(mapping: F[R] => PartialFunction[Option[Int], F[ST2]]): F[R**Int] => F[ST2] =
    f => f.lookupSwitch(f.stack.top, f.stack.rest)(candidates: _*)(mapping)

  /*
   * Here is an alternative signature which uses Tuples instead of PartialFunctions:
   *
  def tableSwitch[R <: Stack, ST2 <: Stack](mapping: F[R] => scala.Stack[(Int, () => F[ST2])]): F[R**Int] => F[ST2] = null

  tableSwitch(f => scala.Stack(
              0 -> (() => f ~ ldc("unbekannte Zahl")),
              1 -> (() => f ~ ldc("eins")),
              5 -> (() => f ~ ldc("fuenf"))))
   */
}
