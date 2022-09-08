from hashlib import blake2b
from api.validation import isTxConserved, doesPkhOwnInputs, didPkhSignTx
from api.models import Entry, Account, UTxO, Value, Token
from api.contracts import Contract
from cbor2 import dumps
from secrets import randbelow

def merkleTree(txIds:list) -> str:
    """
    Calculate the merkle tree from a list of tx ids.
    """

    numberOfTx = len(txIds)
    if numberOfTx == 0:
        return ''
    if numberOfTx == 1:
        return hashTxBody(txIds[0])
    
    # catch bad data types
    if type(txIds) != list:
        return ''
    numberOfTx = len(txIds)
    if numberOfTx == 0:
        return ''
    # account for even or odd lengths
    if numberOfTx % 2 == 0:
        pairs = [(txIds[i], txIds[i+1]) for i in range(0, numberOfTx, 2)]
    else:
        pairs = [(txIds[i], txIds[i+1]) for i in range(0, numberOfTx-1, 2)]
    # build up leaves of the tree
    hashes = []
    for a, b in pairs:
        hashes.append(hashTxBody(str(a)+str(b)))
    # Return the root of the tree
    if len(hashes) == 1:
        return hashes[0]
    # correct for odd lengths
    if numberOfTx % 2 != 0:
        hashes.append(txIds[-1])
    return merkleTree(hashes)

def constructTxBody(inputs:dict, outputs:dict, fee:int) -> str:
    """
    Create a cbor payload of the txBody.
    """
    if fee < 0:
        return ''
    if type(inputs) != dict or type(outputs) != dict:
        return ''
    if len(inputs) == 0 or len(outputs) == 0:
        return ''
    txBody = {
        'inputs': inputs,
        'outputs': outputs,
        'fee': fee
    }
    return dumps(txBody).hex()

def validateTxWrapper(data:list) -> bool:
    """
    Validate a transaction given tx data.
    """
    txBody = data[0]
    txSign = data[1]
    func_name = data[2]

    # BODY
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
    
    # fee cant be 0, asssume min fee is always 1
    try:
        fee = int(txBody['fee'])
        if fee < 1:
            return False
    except KeyError:
        return False

    # Check transaction
    if isTxConserved(inputs, outputs, fee, func_name) is False:
        return False

    # SIGNATURE
    # at least one signer
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
        
        # check for correct tx constructure
        if constructTxBody(inputs, outputs, fee) != txId:
            return False
        
        signature = sig['sig']
        key = sig['key']

        totalInputs = doesPkhOwnInputs(pkh, inputs, totalInputs)

        # finally check if the sig is true
        if txId not in signature:
            return False
        outcome = didPkhSignTx(signature, key)
        outcome = outcome.replace('\n', '')
        if outcome != pkh:
            return False
    
    # Missing Signers
    if totalInputs != 0:
        return False
    
    # CONTRACT
    func_name = data[2]
    contract = Contract()
    return getattr(contract, func_name)(inputs, outputs)

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
    """
    Find an account if it exists. Loop all the utxos and check for correct
    data inputs. Each utxo creates a value that is attached an account as
    an entry.

    Accounts that can not be found fail new utxos.
    """
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
        
        # make sure the pid object is correct
        try:
            pid = list(data.keys())[0]
        except IndexError:
            return False
        
        # must be dict
        if type(data[pid]) != dict:
            return False
        
        # can this fail?
        name = list(data[pid].keys())[0]
        t = Token(pid=pid, name=name)
        t.save()
        
        # positive non zero integers
        if int(data[pid][name]) < 1:
            return False
        
        v = Value(token=t , amount=int(data[pid][name]))
        v.save()
        
        u = UTxO.objects.create(txId=str(utxo))
        u.value.set([v])
        
        e = Entry(account=a, utxo=u)
        e.save()
    return True

def deleteUtxosWrapper(pkh:str, utxos:list) -> bool:
    """
    Delete every entry object that matches this account and is attached to a
    utxo contained in the list of utxos.

    Failing here means no account was found.
    """
    # the account must exist
    try:
        acct = Account.objects.get(pkh=pkh)
    except:
        return False
    
    # delete if found else just pass right by
    for entry in Entry.objects.filter(account=acct):
        if str(entry.utxo.txId) in utxos:
                entry.utxo.delete()
    return True

def hashTxBody(txBody:dict) -> str:
    """
    Input a dictionary and output the blake2b hash. Force digest to 32.
    """
    return blake2b(bytes(str(txBody).encode('utf-8')), digest_size=32).hexdigest()

def randomNumber() -> int:
    """
    Return a random integer that is below the max on-chain integer value.
    """
    return randbelow(pow(2, 64) - 1)