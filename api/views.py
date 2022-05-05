from api.models import Entry, Account, UTxO, Task
from api.validation import didPkhSignTx, isTxConserved, doesPkhOwnInputs
from api.helper import hashTxBody, deleteUtxosWrapper, newUtxosWrapper, sendTxWrapper, validateTxWrapper, merkleTree
from rest_framework import viewsets
from rest_framework import permissions
from api.serializers import EntrySerializer, TaskSerializer
from rest_framework.decorators import action
from rest_framework.response import Response
from cbor2 import dumps, loads

good = { 'status': 200, 'data': '' }
bad  = { 'status': 400, 'data': 'Bad Data' }
failed = { 'status': 400, 'data': 'Tx Failed' }
accountExist  = { 'status': 409, 'data': 'Account Already Exists' }
utxoExist  = { 'status': 409, 'data': 'UTxO Already Exists' }
noAccount  = { 'status': 401, 'data': 'No Account Exists' }

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

        Payload Format:
        """
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        # check payload
        try:
            num = int(data['number'])
            if num < 0:
                bad['data'] = "Missing Data"
                return Response(bad)
            pkhs = data['pkhs']
            if type(pkhs) != list:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            if len(pkhs) == 0:
                bad['data'] = "Missing Data"
                return Response(bad)
            if any(len(ele) == 0 for ele in pkhs) is True:
                bad['data'] = "Missing Data"
                return Response(bad)
            cbor = data['cbor']
            if type(cbor) != str:
                bad['data'] = 'Wrong Data Type'
                return Response(bad)
            if cbor == '':
                bad['data'] = "Missing Data"
                return Response(bad)
        except KeyError:
            bad['data'] = "Missing Data"
            return Response(bad)
        # add data the task db
        t = Task.objects.create(number=num,cbor=cbor)
        acts = []
        for pkh in pkhs:
            try:
                acts.append(Account.objects.get(pkh=pkh))
            except:
                pass
        t.account.set(acts)
        
        good['data'] ='Success'
        return Response(good)
    
    # merkle tree of a singleusers transactions
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getMerkleTree(self, request):
        """
        /tasks/getMerkleTree/

        @see: api.tests.TaskApiTest

         Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        # No empty pkh
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        pkh = data['pkh']
        if pkh == '':
            bad['data'] = "Missing Data"
            return Response(bad)
        payload = []
        for task in Task.objects.all():
            accounts = task.account.all()
            for account in accounts:
                if str(account.pkh) == pkh:
                    payload.append((task.cbor, task.number))
        payload.sort(key=lambda y: y[1])
        txIds = [a for a,_ in payload]
        good['data'] = merkleTree(txIds)
        return Response(good)

    # get all the tasks
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getAll(self, request):
        """
        /tasks/getAll/

        @see: api.tests.TaskApiTest

        No Payload
        """
        payload = []
        # print(Task.objects.all()[0].account.all())
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

    # create a new account
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def newAccount(self, request):
        """
        /entries/newAccount/

        @see: api.tests.NewAccountApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        # No empty pkh
        pkh = str(request.POST['payload'])
        if pkh == '':
            bad['data'] = "Missing Data"
            return Response(bad)
        # Unique accounts only
        try:
            Account(pkh=pkh).save()
        except:
            return Response(accountExist)
        good['data'] ='Success'
        return Response(good)

    # create a new entry
    @action(methods=['post'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def newUTxO(self, request):
        """
        /entries/newUTxO

        @see: api.tests.NewUTxOApiTest

        Payload Format: CBOR
        
        {pkh, utxos} -> {'pkh':'', utxos:{'utxo1': {pid1:{name1:amt1}}, 'utxo2': {pid2:{name2:amt2}}}
        """
        # Dict of utxos and amounts
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        # check data type
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        pkh = str(data['pkh'])
        utxos = data['utxos']
        # must have utxos
        if len(utxos) == 0:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        # validate
        if newUtxosWrapper(pkh, utxos) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            bad['data'] = 'Fail'
            return Response(bad)
    
    # deletes utxos
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def deleteUTxOs(self, request):
        """
        /entries/deleteUTxOs/

        @see: api.tests.DeleteUTxOsApiTest

        Payload Format: CBOR
        
        {pkh, utxos} -> {'pkh':'', utxos: ['utxo1', 'utxo2']}
        """
        # List of UTxOs to Delete from an Account
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        # check for correct data structure
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        pkh = str(data['pkh'])
        utxos = data['utxos']
        if deleteUtxosWrapper(pkh, utxos) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            return Response(noAccount)
    
    # Return all utxos from a single pkh
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def getUTxOs(self, request):
        """
        /entries/getUTxOs/

        @see: api.tests.GetUTxOsApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        utxos = {}
        try:
            acct = Account.objects.get(pkh=request.POST['payload'])
        except:
            return Response(noAccount)
        for entry in Entry.objects.filter(account=acct):
            value = entry.utxo.value
            for val in value.all():
                tkn = val.token
                amt = val.amount
                # dict of txid to value
                utxos[entry.utxo.txId] = {tkn.pid: {tkn.name: amt}}
        # return 200 and the payload
        good['data'] = utxos
        return Response(good)
    
    # Return all utxos from a single pkh
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def totalAda(self, request):
        """
        /entries/totalAda/

        @see: api.tests.TotalAdaApiTest

        Payload Format: Public Key Hash (hexdecimal)

        Chain Agnostic PKH
        """
        total = 0
        # Check if account exists
        try:
            acct = Account.objects.get(pkh=request.POST['payload'])
        except:
            return Response(noAccount)
        for entry in Entry.objects.filter(account=acct):
            for value in entry.utxo.value.all():
                if str(value.token.pid) == "": # synthetic ada only
                    total += value.amount
        # return 200 and the payload
        good['data'] = total
        return Response(good)
    
    # hash some object
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def hashTx(self, request):
        """
        /entries/hashTx/

        @see: api.tests.HashingApiTest

        Payload Format: CBOR

        {inputs, outputs, fee} -> {inputs:{}, outputs:{}, fee:0}
        """
        # List of UTxOs to Delete from an Account
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        # data structure check
        if type(data) != dict:
            bad['data'] = 'Wrong Data Type'
            return Response(bad)
        if len(data) != 3:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        hashedData = hashTxBody(data)
        # attach payload and return
        good['data'] = hashedData
        return Response(good)
    
    # validates a transaction
    @action(methods=['POST'], detail=False, permission_classes=[permissions.IsAuthenticated])
    def validate(self, request):
        """
        /entries/validate/

        @see: api.tests.ValidateApiTest

        Payload Format: CBOR
        
        [txBody, [txSign], contract] -> [{inputs:{}, outputs:{}, fee:0}, [{pkh:"", data:"", sig:""}], 'always_succeed']
        """
        # List of UTxOs to Delete from an Account
        data = str(request.POST['payload'])
        data = loads(bytes.fromhex(data))
        # data structure check
        if type(data) != list:
            bad["data"] = "Wrong Data Type"
            return Response(bad)
        # must contain the body and sig
        if len(data) != 3:
            bad['data'] = 'Missing Fields'
            return Response(bad)
        # validate the data
        if validateTxWrapper(data) is True:
            good['data'] = 'Success'
            return Response(good)
        else:
            bad['data'] = 'Fail'
            return Response(bad)

