package fhetest.Generate

import fhetest.Generate.Utils.InvalidFilterIdx

case class T2Program(
  content: String,
  libConfig: LibConfig,
  invalidFilterIdxList: List[InvalidFilterIdx],
)
