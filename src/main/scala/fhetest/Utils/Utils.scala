package fhetest.Utils

def parsePrefixedArg(input: String): Option[String] = {
  Option(input).collect { case s if s.startsWith("--") => s.stripPrefix("--") }
}

def parseBackend(backendString: String): Option[String] =
  parsePrefixedArg(backendString)

def getWorkspaceDir(backend: String): String = backend match {
  case "SEAL"    => fhetest.SEAL_DIR
  case "OpenFHE" => fhetest.OPENFHE_DIR
  case _         => throw new Error(s"Unknown backend: $backend")
}
