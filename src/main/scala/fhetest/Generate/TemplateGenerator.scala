package fhetest.Generate

import scala.util.Random

// Template Generation Strategy
enum Strategy:
  case Exhaustive, Random

extension (s: Strategy)
  def getGenerator: TemplateGenerator = s match {
    case Strategy.Exhaustive => ExhaustiveGenerator
    case Strategy.Random     => RandomGenerator
  }

// Template Generator
trait TemplateGenerator {
  def generateTemplates(): LazyList[Template]
}

object ExhaustiveGenerator extends TemplateGenerator {
  def generateTemplates(): LazyList[Template] = {
    def allTemplatesOfSize(n: Int): LazyList[Template] = n match {
      case 1 => allAbsStmts.map(stmt => List(stmt))
      case _ =>
        for {
          stmt <- allAbsStmts
          program <- allTemplatesOfSize(n - 1)
        } yield stmt :: program
    }
    LazyList.from(1).flatMap(allTemplatesOfSize)
  }

}

object RandomGenerator extends TemplateGenerator {
  def generateTemplates(): LazyList[Template] = {
    def randomTemplateOfSize(n: Int): Template = {
      (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
    }
    LazyList.from(1).map(randomTemplateOfSize)
  }
}
