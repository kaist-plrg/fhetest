package fhetest

import fhetest.Utils.*
import fhetest.Generate.Strategy
import Generate.LibConfig

class Config(
  var fileName: Option[String] = None,
  var dirName: Option[String] = None,
  var backend: Option[Backend] = None,
  var wordSize: Option[Int] = None,
  var encParams: EncParams = EncParams(32768, 5, 65537),
  var encType: Option[ENC_TYPE] = None,
  var genStrategy: Option[Strategy] = None,
  var genCount: Option[Int] = None,
  var toJson: Boolean = false,
  var sealVersion: Option[String] = None,
  var openfheVersion: Option[String] = None,
  var libConfigOpt: Option[LibConfig] = None,
  var fromJson: Option[String] = None,
  var filter: Boolean = true,
  var silent: Boolean = false,
  var debug: Boolean = false,
  var timeLimit: Option[Int] = None,
)

object Config {
  // each argument is formatted as "-key1:value1 -key2:value2 ..."
  def apply(args: List[String]): Config = {
    val config = new Config()
    args.foreach {
      case arg if arg.startsWith("-") =>
        val Array(key, value) = arg.drop(1).split(":", 2)
        key.toLowerCase() match {
          case "file" => config.fileName = Some(value)
          case "dir"  => config.dirName = Some(value)
          case "b"    => config.backend = parseBackend(value)
          case "w"    => config.wordSize = Some(value.toInt)
          case "n" =>
            config.encParams = config.encParams
              .copy(ringDim = value.toInt)
          case "d" =>
            config.encParams = config.encParams
              .copy(mulDepth = value.toInt)
          case "m" =>
            config.encParams = config.encParams
              .copy(plainMod = value.toInt)
          case "type"  => config.encType = parseEncType(value)
          case "stg"   => config.genStrategy = parseStrategy(value)
          case "count" => config.genCount = Some(value.toInt)
          case "json"  => config.toJson = value.toBoolean
          case "seal" =>
            if SEAL_VERSIONS.contains(value) then
              config.sealVersion = Some(value)
            else
              throw new Error(
                s"Unknown SEAL version: $value, use one of ${SEAL_VERSIONS.mkString(", ")}",
              )
          case "openfhe" =>
            if OPENFHE_VERSIONS.contains(value) then
              config.openfheVersion = Some(value)
            else
              throw new Error(
                s"Unknown OpenFHE version: $value, use one of ${OPENFHE_VERSIONS.mkString(", ")}",
              )
          // FIXME: temperalily added
          case "libconfig" =>
            config.libConfigOpt = Some(LibConfig())
          case "fromjson" => config.fromJson = Some(value)
          case "filter"   => config.filter = value.toBoolean
          case "silent"   => config.silent = value.toBoolean
          case "debug"    => config.debug = value.toBoolean
          case "timeout"  => config.timeLimit = Some(value.toInt)
          case _          => throw new Error(s"Unknown option: $key")
        }
      case _ => // 잘못된 형식의 인자 처리
    }
    config
  }
}
