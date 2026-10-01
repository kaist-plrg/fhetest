#include "openfhe.h"

#include <exception>
#include <iostream>

int main() {
    try {
        lbcrypto::CCParams<lbcrypto::CryptoContextCKKSRNS> parameters;
        parameters.SetMultiplicativeDepth(-1);
        lbcrypto::GenCryptoContext(parameters);
        std::cerr << "Negative depth was accepted\n";
        return 1;
    } catch (const std::exception& error) {
        std::cout << "Negative depth rejected: " << error.what() << '\n';
        return 0;
    }
}
