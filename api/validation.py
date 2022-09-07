from api.models import Entry, Account, UTxO
import subprocess

def didPkhSignTx(sig:str, key:str) -> str:
    """
    Use a sub process to run a node script that will verify the signed data
    using the cip08 library. A successful signature will return the signer
    of the data. This can be used to confirm the pkh of the signer in detail.
    """
    # sig must sign the txId which equals hashTxBody(txBody)
    # pkh on sig must equal pkh provided
    func = [
        'node',
        'api/verify-signature.js',
        sig,
        key
    ]
    return subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8')
    

def doesPkhOwnInputs(pkh:str, inputs:dict, amount: int) -> bool:
    """
    Check if the pkh provided in a transactions owns the inputs.
    """
    try:
        for entry in Entry.objects.filter(account=Account.objects.get(pkh=pkh)):
            if str(entry.utxo.txId) in inputs:
                    amount -= 1
    except:
        return amount
    return amount

def isTxConserved(inputs:dict, outputs:dict, fee:int, contract:str) -> bool:
    """
    Check the inputs, outputs, and fee for transaction conservation.

    Create an dict for all the inputs then a dict for the outputs and fee.
    Subtract out the outputs and fee from the inputs and check that all
    the values are zero.
    """
    inputTotal = {}
    outputTotal = {}
    # Sum inputs
    for txId in inputs:
        utxo = UTxO.objects.get(txId=txId)
        if len(utxo.datum.all()) != 0:
            if contract == "always_succeed":
                return False
        for d in utxo.value.all():
            try:
                inputTotal[d.token.pid][d.token.name] += d.amount
            except KeyError:
                inputTotal[d.token.pid] = {}
                inputTotal[d.token.pid][d.token.name] = d.amount
    # Sum ouputs
    for pkh in outputs:
        try:
            outputs[pkh].pop('data')
        except (KeyError, AttributeError):
            pass

        for pid in outputs[pkh]:
            for tkn in outputs[pkh][pid]:
                try:
                    outputTotal[pid][tkn] += outputs[pkh][pid][tkn]
                except KeyError:
                    outputTotal[pid] = {}
                    outputTotal[pid][tkn] = outputs[pkh][pid][tkn]

        # outputTotal = outputTotal | outputs[pkh]
    feeTotal = {"":{"":fee}}

    for pid in outputTotal:
        for tkn in outputTotal[pid]:
            try:
                inputTotal[pid][tkn] -= outputTotal[pid][tkn]
            except KeyError:
                return False
    for pid in feeTotal:
        for tkn in feeTotal[pid]:
            try:
                inputTotal[pid][tkn] -= feeTotal[pid][tkn]
            except KeyError:
                return False
    for pid in feeTotal:
        for tkn in feeTotal[pid]:
            if inputTotal[pid][tkn] != 0:
                return False
    return True

