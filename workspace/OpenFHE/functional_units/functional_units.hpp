#ifndef FUNCTIONAL_UNITS_HPP_
#define FUNCTIONAL_UNITS_HPP_

#include "openfhe.h"
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <memory>
#include <mutex>
#include <math.h>
#include <numeric>
#include <random>
#include <sstream>
#include <string>
#include <thread>
#include <vector>
#include <cassert>

typedef enum scheme_t {
  BFV, BGV, CKKS, TFHE, NONE
} scheme_t;

using namespace lbcrypto;


#endif  // FUNCTIONAL_UNITS_HPP_
