package fhetest

import fhetest.Utils.*

import java.util.stream.Stream
import scala.jdk.CollectionConverters._

case class LibConfig(
  scheme: Scheme = Scheme.CKKS,
  encParams: EncParams = EncParams(32768, 5, 65537),
  firstModSize: Int = 60,
  scalingModSize: Int = 40,
  // TODO: securityLevel option is not applied yet in SEAL
  securityLevel: SecurityLevel = SecurityLevel.HEStd_NotSet,
  scalingTechnique: ScalingTechnique = ScalingTechnique.FLEXIBLEAUTOEXT,
  lenOpt: Option[Int] = None,
  boundOpt: Option[Int | Double] = None,
) {

  val sealConfigs: List[String] = sealStr.split("\n").toList
  val openfheConfigs: List[String] = openfheStr.split("\n").toList

  def dumpSeal(): Stream[String] = sealConfigs.asJava.stream()
  def dumpOpenfhe(): Stream[String] = openfheConfigs.asJava.stream()

  // len : if not set, use ringDim / 2 if scheme is CKKS, else use ringDim
  lazy val len: Int = lenOpt.getOrElse(
    if (scheme == Scheme.CKKS) encParams.ringDim / 2
    else encParams.ringDim,
  )
  lazy val bound: Int | Double = boundOpt.getOrElse(
    if (scheme == Scheme.CKKS) 100.0
    else 100,
  )

  lazy val openfheStr: String =
    lazy val dataTyStr =
      if (scheme == Scheme.CKKS) "complex<double>" else "int64_t"
    lazy val tempVecStr = s"vector<$dataTyStr> tmp_vec_($len);"
    s"""CCParams<CryptoContext${scheme}RNS> parameters;
parameters.SetRingDim(${encParams.ringDim});
parameters.SetMultiplicativeDepth(${encParams.mulDepth});
parameters.SetPlaintextModulus(${encParams.plainMod});
parameters.SetFirstModSize(${firstModSize});
parameters.SetScalingModSize(${scalingModSize});
parameters.SetSecurityLevel(${securityLevel});
parameters.SetScalingTechnique(${ScalingTechnique});

CryptoContext<DCRTPoly> cc = GenCryptoContext(parameters);
cc->Enable(PKE);
cc->Enable(KEYSWITCH);
cc->Enable(LEVELEDSHE);
KeyPair<DCRTPoly> keyPair;
keyPair = cc->KeyGen();
cc->EvalMultKeyGen(keyPair.secretKey);
size_t slots($len);
$tempVecStr
Plaintext tmp;
int rots_num = 20;
vector<int> rots(rots_num + 1);
for(int i = 0; i < rots_num + 1; ++i) {
  rots[i] = i;
}
cc->EvalRotateKeyGen(keyPair.secretKey, rots);
Ciphertext<DCRTPoly> tmp_;"""

  lazy val sealStr: String =
    lazy val scaleModsStr = s", ${scalingModSize}" * encParams.mulDepth
    lazy val moduliStr = s"vector<int> { $firstModSize$scaleModsStr, 60 }"
    s"""EncryptionParameters parms(scheme_type::${scheme});
size_t poly_modulus_degree = ${encParams.ringDim};
parms.set_poly_modulus_degree(poly_modulus_degree);
parms.set_coeff_modulus(CoeffModulus::Create(poly_modulus_degree, $moduliStr));
double scale = pow(2.0, $scalingModSize);
SEALContext context(parms);
KeyGenerator keygen(context);
SecretKey secret_key = keygen.secret_key();
PublicKey public_key;
RelinKeys relin_keys;
GaloisKeys gal_keys;
keygen.create_public_key(public_key);
keygen.create_relin_keys(relin_keys);
keygen.create_galois_keys(gal_keys);
Encryptor encryptor(context, public_key);
Evaluator evaluator(context);
Decryptor decryptor(context, secret_key);
CKKSEncoder encoder(context);
size_t slot_count = encoder.slot_count();
Plaintext tmp;
Ciphertext tmp_;
"""
}
