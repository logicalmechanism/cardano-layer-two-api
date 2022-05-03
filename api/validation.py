from api.models import Entry, Account
import subprocess

def didPkhSignTx(sig:str) -> bool:
    # sig must sign the txId which equals hashTxBody(txBody)
    # pkh on sig must equal pkh provided
    func = [
        'node',
        'api/verify-signature.js',
        sig,
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

def isTxConserved(inputs:dict, outputs:dict, fee:int) -> bool:
    """
    Check the inputs, outputs, and fee for transaction conservation.
    """
    inputTotal = 0
    outputTotal = 0
    # Sum inputs
    for utxo in inputs:
        inputTotal += int(inputs[utxo])
    # Sum ouputs
    for utxo in outputs:
        outputTotal += int(outputs[utxo])
    # utxos exist but they are fake
    if inputTotal == 0 or outputTotal == 0:
        return False
    return inputTotal == outputTotal + fee

