package fhetest.Generate

import fhetest.Utils.*

import java.util.stream.Stream
import scala.jdk.CollectionConverters._

case class LibConfig(
  scheme: Scheme = Scheme.CKKS,
  encParams: EncParams = EncParams(32768, 5, 65537),
  firstModSize: Int = 60,
  scalingModSize: Int = 59,
  securityLevel: SecurityLevel = SecurityLevel.HEStd_NotSet,
  scalingTechnique: ScalingTechnique = ScalingTechnique.FLEXIBLEAUTOEXT,
  lenOpt: Option[Int] = None,
  boundOpt: Option[Int | Double] = None,
  rotateBoundOpt: Option[Int] = None,
) {

  def stringify(): String =
    s"""{scheme: ${scheme}}
{encParams: EncParams(${encParams.ringDim}, ${encParams.mulDepth}, ${encParams.plainMod})}
{(firstModSize, scalingModSize): (${firstModSize}, ${scalingModSize})}
{securityLevel: ${securityLevel}}
{scalingTechnique: ${scalingTechnique}}
{lenOpt: ${lenOpt}}
{boundOpt: ${boundOpt}}
{rotateBoundOpt: ${rotateBoundOpt}}
"""

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
    if (scheme == Scheme.CKKS) 1000.0
    else 1000,
  )
  lazy val rotateBound: Int = rotateBoundOpt.getOrElse(10)

  lazy val openfheStr: String =
    lazy val dataTyStr =
      if (scheme == Scheme.CKKS) "complex<double>" else "int64_t"
    // To avoid disabled functions.
    // [References]
    // - src/pke/include/scheme/bfvrns/gen-cryptocontext-bfvrns-params.h
    // - src/pke/include/scheme/bgvrns/gen-cryptocontext-bgvrns-params.h
    // - src/pke/include/scheme/ckksrns/gen-cryptocontext-ckksrns-params.h
    // - src/pke/lib/scheme/gen-cryptocontext-params-validation.cpp
    lazy val modStr =
      if (scheme == Scheme.BFV)
        s"""parameters.SetPlaintextModulus(${encParams.plainMod});
parameters.SetScalingModSize(${scalingModSize});"""
      else if (scheme == Scheme.BGV)
        if (scalingTechnique == ScalingTechnique.FIXEDMANUAL)
          s"""parameters.SetPlaintextModulus(${encParams.plainMod});
parameters.SetFirstModSize(${firstModSize});"""
          s"parameters.SetPlaintextModulus(${encParams.plainMod});"
        else s"parameters.SetPlaintextModulus(${encParams.plainMod});"
      else s"""parameters.SetFirstModSize(${firstModSize});
parameters.SetScalingModSize(${scalingModSize});"""
    lazy val scalingTechStr =
      if (scheme == Scheme.BFV) ""
      else s"parameters.SetScalingTechnique(${scalingTechnique});"
    lazy val tempVecStr = s"vector<$dataTyStr> tmp_vec_($len);"
    s"""CCParams<CryptoContext${scheme}RNS> parameters;
parameters.SetRingDim(${encParams.ringDim});
parameters.SetMultiplicativeDepth(${encParams.mulDepth});
$modStr
parameters.SetSecurityLevel(${securityLevel});
$scalingTechStr

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
    lazy val encoderName =
      if (scheme == Scheme.CKKS) "encoder"
      else "batch_encoder"
    lazy val encoderType =
      if (scheme == Scheme.CKKS) s"CKKSEncoder"
      else "BatchEncoder"
    lazy val slotStr =
      if (scheme == Scheme.CKKS) s"slot_count"
      else "slots"
    lazy val sealSecLevelStr = securityLevel match {
      case SecurityLevel.HEStd_NotSet      => "none"
      case SecurityLevel.HEStd_128_classic => "tc128"
      case SecurityLevel.HEStd_192_classic => "tc192"
      case SecurityLevel.HEStd_256_classic => "tc256"
    }

    lazy val moduliStr =
      s"vector<int> { $firstModSize$scaleModsStr, $firstModSize }"
    lazy val plainModStr =
      if (scheme == Scheme.CKKS) ""
      else s"parms.set_plain_modulus(${encParams.plainMod});"
    s"""EncryptionParameters parms(scheme_type::${scheme
        .toString()
        .toLowerCase()});
size_t poly_modulus_degree = ${encParams.ringDim};
parms.set_poly_modulus_degree(poly_modulus_degree);
parms.set_coeff_modulus(CoeffModulus::Create(poly_modulus_degree, $moduliStr));
$plainModStr
double scale = pow(2.0, $scalingModSize);
SEALContext context(parms, true, sec_level_type::${sealSecLevelStr});
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
$encoderType $encoderName(context);
size_t $slotStr = $encoderName.slot_count();
Plaintext tmp;
Ciphertext tmp_;
"""
}
