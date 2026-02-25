# HEProgTest (FHETEST)
> Note: This framework was previously called **FHETEST**.  
> Although the name has been updated to **HEProgTest** in our paper, some commands (e.g., `fhetest`) still retain the old name.  
> They refer to the same framework.

> Currently, this project is still in the development phase. 

The goal of this project is to test the HE (Homomorphic Encryption) libraries.

## Overall structure
<img width="1706" alt="fhetest_structure" src="framework-figure.png">

## Installation Guide
Our tool is written in Scala3, so you need to install the Scala3 compiler and build tools.
You can find the installation instructions [here](https://docs.scala-lang.org/getting-started/index.html).

### Download this project
```sh
$ git clone https://github.com/kaist-plrg/fhetest.git
```

### Environment Setting
Insert the following commands to ~/.bashrc (or ~/.zshrc):
```sh
export FHETEST_HOME="<path to the fhetest directory>"
export PATH="$FHETEST_HOME/bin:$PATH"
``` 

### Install the submodules
This tool uses the [T2-FHE-Compiler-and-Benchmarks](https://github.com/TrustworthyComputing/T2-FHE-Compiler-and-Benchmarks) with some modifications. So you need to install the submodules.
```sh
$ cd $FHETEST_HOME &&  git submodule update --init
```

### Install the HE libraries
You can install the HE libraries(SEAL, OpenFHE) by running the `build.sh` script in the T2 directory.
```sh
$ cd $FHETEST_HOME/src/main/java/T2-FHE-Compiler-and-Benchmarks/
$ chmod +x .circleci/build_libs.sh
$ .circleci/build_libs.sh
```

### Build the project
Because the T2 project requires [some dependencies](https://github.com/TrustworthyComputing/T2-FHE-Compiler-and-Benchmarks?tab=readme-ov-file#dependencies), you should install them first.
```sh
$ apt install cmake make build-essential g++ clang autoconf javacc patchelf openjdk-8-jdk maven m4 tar lzip libfftw3-dev
```

Now, you can build the project by sbt.
```sh
# build the T2 project and the fhetest project
$ sbt buildT2 && sbt assembly
```

If you want to know more about the commands, you can run the following command:
```sh
$ fhetest help
# Usage: fhetest <command> [options]
# Commands:
# ...
```
