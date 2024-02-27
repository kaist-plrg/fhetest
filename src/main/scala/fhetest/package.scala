package fhetest

/** base project directory root */
val BASE_DIR = settings.ProjectSettings.projectHome

/** log directory */
val LOG_DIR = s"$BASE_DIR/logs"
val TEST_DIR = s"$LOG_DIR/test"

/** T2 directory */
val T2_DIR = s"$BASE_DIR/src/main/java/T2-FHE-Compiler-and-Benchmarks"

/** resource directory */
val RESOURCE_DIR = s"$BASE_DIR/src/main/resources"
val BASIC_TESTSET_DIR = s"$RESOURCE_DIR/basic_test"
val ADVANCED_TESTSET_DIR = s"$RESOURCE_DIR/advanced_test"
val BIN_ADVANCED_TESTSET_DIR = s"$ADVANCED_TESTSET_DIR/binary"
val ARITH_ADVANCED_TESTSET_DIR = s"$ADVANCED_TESTSET_DIR/arithmetic"

/* Workspace directory */
val WORKSPACE_DIR = s"$BASE_DIR/workspace"
val SEAL_DIR = s"$WORKSPACE_DIR/SEAL"
val OPENFHE_DIR = s"$WORKSPACE_DIR/OpenFHE"

val SEAL_VERSIONS = List("4.1.1", "4.1.0", "4.0.0", "3.7.3", "3.7.2")
val OPENFHE_VERSIONS = List("1.1.2", "1.0.4", "1.0.3", "1.0.2", "1.0.1")
