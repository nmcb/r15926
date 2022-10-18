object R15926 extends App:

  def main(): Unit =
    println(summon[Sum[Minus[Succ[Succ[Zero]]], Minus[Succ[Succ[Zero]]]] =:= Minus[Succ[Succ[Succ[Succ[Zero]]]]]])

  sealed trait IntT
  sealed trait NatT extends IntT
  final case class Zero() extends NatT
  final case class Succ[+N <: NatT](n: N) extends NatT
  final case class Minus[+N <: Succ[NatT]](n: N) extends IntT

  type NatSum[X <: NatT, Y <: NatT] <: NatT = Y match
    case Zero => X
    case Succ[Zero] => NatSum[Succ[X], Zero]
    case Succ[y] => NatSum[Succ[X], y]

  type NatDif[X <: NatT, Y <: NatT] <: IntT = Y match
    case Zero => X
    case Succ[y] => X match
      case Zero => Minus[Y]
      case Succ[x] => NatDif[x, y]

  type Sum[X <: IntT, Y <: IntT] <: IntT = Y match
    case Zero => X
    case Minus[y] => X match
      case Minus[x] => Minus[NatSum[x, y]]
      case _ => NatDif[X, y]
    case _ => X match
      case Minus[x] => NatDif[Y, x]
      case _ => NatSum[X, Y]
