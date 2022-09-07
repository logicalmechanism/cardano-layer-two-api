import * as cip08 from '@stricahq/cip08'

const signedCBOR = process.argv[2] || null;
const keyedCBOR = process.argv[3] || null;
const writeResponse = (value) => process.stdout.write(value);
const error = () => writeResponse('');

if (!signedCBOR) { error(); }
if (!keyedCBOR) { error(); }

try {
    const builder = cip08.default.CoseSign1.fromCbor(signedCBOR);
    const pkBuffer = cip08.getPublicKeyFromCoseKey(keyedCBOR);
    builder.verifySignature({publicKeyBuffer: pkBuffer}) ? console.log(Buffer.from(builder.getAddress()).toString("hex")) : error();
} catch (e) {
    console.log(e);
    error();
}