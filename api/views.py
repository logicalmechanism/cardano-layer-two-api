from api.models import Entry, Account, UTxO, Task
from api.validation import didPkhSignTx, isTxConserved, doesPkhOwnInputs
from api.helper import hashTxBody, deleteUtxosWrapper, newUtxosWrapper, sendTxWrapper, validateTxWrapper, merkleTree, randomNumber
from rest_framework import viewsets
from rest_framework import permissions
from api.serializers import EntrySerializer, TaskSerializer
from rest_framework.decorators import action
from rest_framework.response import Response
from cbor2 import dumps, loads
###############################################################################
#
###############################################################################

good         = { 'status': 200, 'data': '' }
bad          = { 'status': 400, 'data': 'Bad Data' }
failed       = { 'status': 400, 'data': 'Tx Failed' }
accountExist = { 'status': 409, 'data': 'Account Already Exists' }
utxoExist    = { 'status': 409, 'data': 'UTxO Already Exists' }
noAccount    = { 'status': 401, 'data': 'No Account Exists' }

###############################################################################
#
###############################################################################
class TaskViewSet(viewsets.ModelViewSet):
    """
    API endpoints for the task db of tasks.
    """
    queryset = Task.objects.all()
    serializer_class = TaskSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    # create a new task
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def newTask(self, request):
        """
        /tasks/newTask/

        @see: api.tests.TaskApiTest

        Payload Format: CBOR

        [txBody, [txSign], contract] -> [
            {inputs:{}, outputs:{}, fee:0}, 
            [
                {pkh:"", data:"tx_body_hash", sig:"pkh_sig_of_data", "key":"key_of_sig"}
            ], 
            'smart_contract'
            ]

        inputs  -> {'tx_hash#1':{}, 'tx_hash#2':{}}
        outputs -> {
            'pkh1':{'pid1':{'tkn1':amt1}, 'pid2':{'tkn2':amt2}, 'data':{}},
            'pkh2':{'pid1':{'tkn1':amt1}, 'data':{}}
            }
        """
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)

        data = loads(bytes.fromhex(data)) # uncbor the data

        # check payload
        try:
            num = int(data['number'])
            if num < 0:
                bad['data'] = "Missing Data"
                return Response(bad)
            
            cbor = data['cbor']
            if type(cbor) != str:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            
            if cbor == '':
                bad['data'] = "Missing Data"
                return Response(bad)
            
            try:
                cbor_data = loads(bytes.fromhex(cbor)) # uncbor the data
            except ValueError:
                bad["data"] = "Wrong Data Type"
                return Response(bad)
                
            # data structure check
            if type(cbor_data) != list:
                bad["data"] = "Wrong Data Type"
                return Response(bad)
            
            # must contain the body and sig
            if len(cbor_data) != 3:
                bad['data'] = 'Missing Fields'
                return Response(bad)
            
            # check sub field types
            if type(cbor_data[0]) != dict:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            if type(cbor_data[1]) != list:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            if type(cbor_data[2]) != str:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            
            # validate the data
            if validateTxWrapper(cbor_data) is False:
                bad['data'] = 'Fail'
                return Response(bad)
            
            # the db update based off of the cbor here
            inputs  = [utxo for utxo in cbor_data[0]['inputs']]
            deleteData = {}
            for signature in cbor_data[1]:
                pkh = signature['pkh']
                deleteData[pkh] = []
                for entry in Entry.objects.filter(account=Account.objects.get(pkh=pkh)):
                    txId = entry.utxo.txId
                    if txId in inputs:
                        deleteData[pkh].append(txId)

            for p in deleteData:
                deleteUtxosWrapper(p, deleteData[p])

            thisTxId = hashTxBody(cbor_data[0])
            counter = 0
            for out in cbor_data[0]['outputs']:
                newUtxosWrapper(out, {thisTxId+'#'+str(counter): cbor_data[0]['outputs'][out]})
                counter += 1
            # delete inputs and create outputs
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        # add data the task db
        Task.objects.create(number=num,cbor=cbor)
        
        good['data'] ='Success'
        return Response(good)
    
    # get all the tasks
    @action(methods=['get'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getAll(self, request):
        """
        /tasks/getAll/

        @see: api.tests.TaskApiTest

        No Payload
        """
        payload = []
        for task in Task.objects.all():
            payload.append((task.cbor, task.number))
        payload.sort(key=lambda y: y[1])
        good['data'] = payload
        return Response(good)

class EntryViewSet(viewsets.ModelViewSet):
    """
    API endpoints for the state db of entries.
    """
    queryset = Entry.objects.all()
    serializer_class = EntrySerializer
    permission_classes = [permissions.IsAuthenticated]

    # create a new account with a pkh
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def newAccount(self, request):
        """
        /entries/newAccount/

        @see: api.tests.NewAccountApiTest

        Payload Format: Public Key Hash (hexdecimal)

        length 56, Chain Agnostic PKH
        """
        # check for missing data
        try:
            pkh = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)

        # payment public key hash only, length 56
        if len(pkh) < 56:
            bad['data'] = "Incorrect Length Key"
            return Response(bad)
        
        # Unique accounts only
        try:
            Account(pkh=pkh).save()
        except:
            return Response(accountExist)
        
        # good to go
        good['data'] ='Success'
        return Response(good)

    # create a new entry
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def newUTxO(self, request):
        """
        /entries/newUTxO/

        @see: api.tests.NewUTxOApiTest

        Payload Format: CBOR
        
        {pkh, utxos} -> {'pkh':'', 'utxos':{'tx_hash#1': {'pid1':{'tkn1':amt1}}, 'tx_hash#2': {'pid2':{'tkn2':amt2}}}

        The UTxO tx hashes are blake2b. Pids and tkns follow layer1 conventions.
        """
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        # Dict of utxos and amounts
        data = loads(bytes.fromhex(data))
        
        # check data type
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        
        # check if both keys are there
        try:
            pkh = str(data['pkh']) # force string
            utxos = data['utxos']
        except KeyError:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        
        if type(utxos) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)

        # must have utxos
        if len(utxos) == 0:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        
        # validate the incoming utxo
        if newUtxosWrapper(pkh, utxos) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            # this fail is like 3+ different possible failures.
            bad['data'] = 'Fail'
            return Response(bad)
    
    # deletes a list utxos
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def deleteUTxOs(self, request):
        """
        /entries/deleteUTxOs/

        @see: api.tests.DeleteUTxOsApiTest

        Payload Format: CBOR
        
        {pkh, utxos} -> {'pkh':'', 'utxos': ['tx_hash#1', 'tx_hash#2']}

        Delete a list of utxos from some account.
        """
        # may sure there is data
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        # List of UTxOs to Delete from an Account
        data = loads(bytes.fromhex(data))
        
        # check for correct data structure
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        
        # check if object has correct data
        try:
            pkh = str(data['pkh'])
            utxos = data['utxos']
        except KeyError:
            bad['data'] = 'Missing Fields'
            return Response(bad)

        if deleteUtxosWrapper(pkh, utxos) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            # this can only fail wit a no account
            return Response(noAccount)
    
    # Return all utxos from a single pkh
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getUTxOs(self, request):
        """
        /entries/getUTxOs/

        @see: api.tests.GetUTxOsApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH

        Returns all UTxOs attached to some account.
        """
        # check for missing data
        try:
            pkh = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        # must have an account to get utxos
        # for a in Account.objects.all():
        #     print(a.pkh)
        try:
            acct = Account.objects.get(pkh=pkh)
        except:
            return Response(noAccount)
        # loop the entries for the account and return a value of all the data
        utxos = {}
        for entry in Entry.objects.filter(account=acct):
            # loop all tokens in value
            value = entry.utxo.value
            for val in value.all():
                tkn = val.token
                amt = val.amount
                # dict of txid to value
                utxos[entry.utxo.txId] = {tkn.pid: {tkn.name: amt}}
        
        # return 200 and the payload
        good['data'] = utxos
        return Response(good)
    
    # Return the total ada from a single pkh
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def totalAda(self, request):
        """
        /entries/totalAda/

        @see: api.tests.TotalAdaApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        
        # check for missing data
        try:
            pkh = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        # must have an account to get utxos
        try:
            acct = Account.objects.get(pkh=pkh)
        except:
            return Response(noAccount)
        
        # get total ada
        total = 0
        for entry in Entry.objects.filter(account=acct):
            for value in entry.utxo.value.all():
                # synthetic ada only
                if str(value.token.pid) == "":
                    total += value.amount
        
        # return 200 and the payload
        good['data'] = total
        return Response(good)
    
    # hash some tx body object
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def hashTx(self, request):
        """
        /entries/hashTx/

        @see: api.tests.HashingApiTest

        Payload Format: CBOR

        {inputs, outputs, fee} -> {inputs:{}, outputs:{}, fee:0}

        inputs  -> {'tx_hash#1':redeemer1, 'tx_hash#2':{}}
        outputs -> {
            'pkh1':{'pid1':{'tkn1':amt1}, 'pid2':{'tkn2':amt2}, data:{}},
            'pkh2':{'pid2':{'tkn2':amt2}, data:datum2}
        }
        """
        # check for missing data
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)

        data = loads(bytes.fromhex(data))

        # data structure check
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if len(data) != 3:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        
        # make sure fields exist
        try:
            data['inputs']
            data['outputs']
            data['fee']
        except KeyError:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        
        # check sub field types
        if type(data['inputs']) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if type(data['outputs']) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if type(data['fee']) != int:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)

        hashedData = hashTxBody(data)
        # attach payload and return
        good['data'] = hashedData
        return Response(good)
    
    # returns a random integer less than 2^64 - 1
    @action(methods=['GET'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def randN(self, request):
        """
        /entries/randN/

        @see: api.tests.randN
        """
        # attach payload and return
        good['data'] = randomNumber()
        return Response(good)

    # validates a transaction
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def validate(self, request):
        """
        /entries/validate/

        @see: api.tests.ValidateApiTest

        Payload Format: CBOR
        
        [txBody, [txSign], contract] -> [
            {inputs:{}, outputs:{}, fee:0}, 
            [
                {pkh:"", data:"tx_body_hash", sig:"pkh_sig_of_data", "key":"key_of_sig"}
            ], 
            'smart_contract'
            ]

        inputs  -> {'tx_hash#1':redeemer1, 'tx_hash#2':redeemer2}
        outputs -> {
            'pkh1':{'pid1':{'tkn1':amt1}, 'pid2':{'tkn2':amt2}, 'data':datum1},
            'pkh2':{'pid1':{'tkn1':amt1}, 'data':{}}
            }
        
        The inputs have redeemers assigned to them to use for spending inside 
        the smart contract and each output has an optional data attached 
        as the outputs datum. The signing object allows for a multisig to exist 
        at the time of spending.
        """
        # List of UTxOs to Delete from an Account
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        data = loads(bytes.fromhex(data))
        # data structure check
        if type(data) != list:
            bad["data"] = "Wrong Data Type"
            return Response(bad)
        
        # must contain the body and sig
        if len(data) != 3:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        
        # check sub field types
        if type(data[0]) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if type(data[1]) != list:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if type(data[2]) != str:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)

        # validate the data
        if validateTxWrapper(data) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            bad['data'] = 'Fail'
            return Response(bad)

    # merkle tree of a single users transactions
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getMerkleTree(self, request):
        """
        /entries/getMerkleTree/

        @see: api.tests.MerkleTreeApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        # No empty pkh
        try:
            data = str(request.POST['payload'])
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        
        if data == '':
            bad['data'] = "Missing Data"
            return Response(bad)

        payload = []
        entries = Entry.objects.all()
        for entry in entries:
            if str(entry.account.pkh) == data:
                payload.append(entry.utxo.txId)

        # get merkle and return
        good['data'] = merkleTree(payload)
        return Response(good)