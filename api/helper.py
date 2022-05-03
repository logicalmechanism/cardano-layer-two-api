from hashlib import blake2b
from api.validation import isTxConserved, doesPkhOwnInputs, didPkhSignTx
from api.models import Entry, Account, UTxO, Value, Token
from cbor2 import dumps

def constructTxBody(inputs:dict, outputs:dict, fee:int) -> str:
    txBody = {
        'inputs': inputs,
        'outputs': outputs,
        'fee': fee
    }
    return dumps(txBody).hex()

def validateTxWrapper(data:list) -> bool:
    # BODY
    txBody = data[0]
    if type(txBody) != dict:
        return False
    
    # must have at least 1 input
    try:
        inputs = txBody['inputs']
    except KeyError:
        return False
    if len(inputs) == 0:
        return False
    
    # must have at least 1 output
    try:
        outputs = txBody['outputs']
    except KeyError:
        return False
    if len(outputs) == 0:
        return False
    
    # fee cant be 0
    try:
        fee = int(txBody['fee'])
        if fee < 1:
            return False
    except KeyError:
        return False

    # Check transaction
    if isTxConserved(inputs, outputs, fee) is False:
        return False
    
    # SIGNATURE
    txSign = data[1]
    if type(txSign) != list:
        return False
    if len(txSign) < 1:
        return False
    totalInputs = len(inputs)
    for sig in txSign:
        try:
            pkh = str(sig['pkh'])
        except KeyError:
            return False
        if pkh == '':
            return False
        
        try:
            txId = sig['data']
        except KeyError:
            return False
        if hashTxBody(txBody) != txId:
            return False
        
        signature = sig['sig']

        totalInputs = doesPkhOwnInputs(pkh, inputs, totalInputs)
        if txId not in signature:
            return False

        if didPkhSignTx(signature) != pkh:
            return False
    # Passed Validation
    return totalInputs == 0
    # return True

def sendTxWrapper(inputs:dict, outputs:dict, txId:str) -> bool:
    FEE = "feepkhhere"
    outboundUTxOs = {}
    for ind, pkh in enumerate(outputs):
        try:
            outboundUTxOs[pkh][txId+'#'+str(ind)] = outputs[pkh]
        except KeyError:
            outboundUTxOs[pkh] = {}
            outboundUTxOs[pkh][txId+'#'+str(ind)] = outputs[pkh]

    deleteUtxosWrapper(pkh, list(inputs.keys()))
    for out in outboundUTxOs:
        newUtxosWrapper(out, outboundUTxOs[out])
    newUtxosWrapper(FEE, {txId+'#'+str(len(outputs)+1):1})
    return True

def newUtxosWrapper(pkh:str, utxos:dict) -> bool:
    try:
        a = Account.objects.get(pkh=pkh)
    except:
        return False
    # add in all utxos and fail if any fail
    for utxo in utxos:
        data = utxos[utxo]
        # must be dict
        if type(data) != dict:
            return False
        pid = list(data.keys())[0]
        # must be dict
        if type(data[pid]) != dict:
            return False
        name = list(data[pid].keys())[0]
        t = Token(pid=pid, name=name)
        t.save()
        # positive non zero integers
        if int(data[pid][name]) < 1:
            return False
        v = Value(token=t , amount=int(data[pid][name]))
        v.save()
        u = UTxO(txId=str(utxo), value=v)
        u.save() # fails on nonunique
        e = Entry(account=a, utxo=u)
        e.save()
    return True

def deleteUtxosWrapper(pkh:str, utxos:list) -> bool:
    try:
        acct = Account.objects.get(pkh=pkh)
    except:
        return False
    for entry in Entry.objects.filter(account=acct):
        if str(entry.utxo.txId) in utxos:
                entry.utxo.delete()
    return True

def hashTxBody(txBody:dict) -> str:
    """
    Input a dictionary and output the blake2b hash. Force digest to 32.
    """
    return blake2b(bytes(str(txBody).encode('utf-8')), digest_size=32).hexdigest()