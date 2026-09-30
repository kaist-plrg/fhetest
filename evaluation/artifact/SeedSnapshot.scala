import fhetest.Config
import fhetest.Generate.{Strategy, getValidFilterList2str}
import fhetest.Phase.Generate
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.security.MessageDigest
import scala.util.Random

object SeedSnapshot {
  def main(args: Array[String]): Unit = {
    require(args.length == 5, "SeedSnapshot OUTPUT TYPE MODE SEED COUNT")
    val mode = args(2)
    require(Set("valid", "invalid", "baseline").contains(mode))
    val config = Config(List(
      s"-type:${args(1)}", s"-seed:${args(3)}", s"-count:${args(4)}",
      s"-filter:${mode != "invalid"}", s"-nofilter:${mode == "baseline"}"
    ))
    val count = config.genCount.getOrElse(throw new IllegalArgumentException("Missing count"))
    require(count > 0)
    val encType = config.encType.getOrElse(throw new IllegalArgumentException("Use int or double"))
    Random.setSeed(config.seed.getOrElse(throw new IllegalArgumentException("Missing seed")))
    val generator = Generate(encType, Strategy.Random, config.validFilter, config.noFilterOpt)
    val rows = generator(Some(count)).zipWithIndex.map { (program, index) =>
      val serialized = program.content + "\n" + program.libConfig.stringify() +
        "\n" + program.invalidFilterIdxList.mkString(",")
      val digest = MessageDigest.getInstance("SHA-256").digest(serialized.getBytes(UTF_8))
        .map(byte => f"${byte & 0xff}%02x").mkString
      s"$index\t$digest"
    }.toVector
    require(rows.size == count, s"Only ${rows.size} programs generated")
    val snapshot = "filters=" + getValidFilterList2str().mkString(",") + "\n" + rows.mkString("\n") + "\n"
    Files.writeString(Paths.get(args(0)), snapshot, UTF_8, StandardOpenOption.CREATE_NEW)
    println(s"Snapshot written: ${args(0)} ($count programs)")
  }
}
