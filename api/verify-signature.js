import * as cip08 from '@stricahq/cip08'

const signedCBOR = process.argv[2] || null;
const writeResponse = (value) => process.stdout.write(value);
const error = () => writeResponse('');

if (!signedCBOR) { error(); }

try {
    const payloadBuffer = Buffer.from(signedCBOR, 'hex');
    const cipPayload = cip08.default.CoseSign1.fromCbor(payloadBuffer);
    cipPayload.verifySignature() ? writeResponse(Buffer.from(cipPayload.getAddress()).toString("hex").slice(2,58)) : error();
} catch (e) {
    error();
}
