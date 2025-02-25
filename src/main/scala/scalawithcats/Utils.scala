package scalawithcats

/**
 * https://docs.scala-lang.org/overviews/reflection/typetags-manifests.html
 */

import scala.reflect.runtime.{universe => ru}
import scala.reflect.{ClassManifest, ClassTag}
import scala.reflect.runtime.universe._

object Utils {

  private def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

  private def getInstanceType[T: TypeTag](obj: T) = typeOf[T]

  private def getInstanceType1[T: ClassTag](obj: T) = {
    val modelType = implicitly[ClassTag[T]].runtimeClass
    val typeParam =
      modelType.getTypeParameters.map(x => x.getName).mkString("[", ",", "]")
    modelType.getName + typeParam
  }

  def p[T: Manifest](a: T): Unit = {
    val t = manOf(a)
    println(s"$t = $a")
  }

  def p1[T: TypeTag](a: T): Unit = {
    val t = getInstanceType(a)
    println(s"$t = $a")
  }

  def p2[T](a: T): Unit = {
    val t = a.getClass.getName
    println(s"$t = $a")
  }

  def p3[T: ClassTag](a: T): Unit = {
    val t = getInstanceType1(a)
    println(s"$t = $a")
  }

  def p4[T: WeakTypeTag](a: T): Unit = {
    val tag = implicitly[WeakTypeTag[T]]
    val t = tag.tpe
    println(s"$t = $a")
  }

  def p5[T](a: T)(implicit tag: WeakTypeTag[T]): Unit = {
    val t = tag.tpe
    println(s"$t = $a")
  }

}
