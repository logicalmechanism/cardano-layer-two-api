from django.test import RequestFactory, TestCase
from api.views import EntryViewSet, TaskViewSet
from api.models import Entry, Account, UTxO, Value, Token, Task
from api.helper import hashTxBody, merkleTree
from cbor2 import dumps, loads

class TaskApiTest(TestCase):
    """
    Testing the validate api endpoint.
    """
    def setUp(self):
        Account.objects.create(pkh="somepkhhere")
        Account.objects.create(pkh="anotherpkhhere")
    
    def test_new_task(self):
        test_data = dumps({
            "number": 0,
            "cbor": "testing",
            "pkhs": ["somepkhhere"]
        }).hex()
        request = RequestFactory().post('/tasks/newTask/', {'payload': test_data})
        view = TaskViewSet.newTask(self, request)
        t = Task.objects.get(number=0)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(t.cbor, "testing")
    
    def test_account_assign(self):
        user1 = Account.objects.get(pkh="somepkhhere")
        user2 = Account.objects.get(pkh="anotherpkhhere")
        
        task1 = Task.objects.create(number=0, cbor='a')
        task1.account.set([user1, user2])
        
        task2 = Task.objects.create(number=1, cbor='b')
        task2.account.set([user2])
        
        task3 = Task.objects.create(number=2, cbor='c')
        task3.account.set([user1])

        test_data = dumps({
            "pkh":"somepkhhere"
        }).hex()
        request = RequestFactory().post('/tasks/getMerkleTree/', {'payload': test_data})
        view = TaskViewSet.getMerkleTree(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'd2fbd52b2bd2ae1372cc2aaa2b22908cb2ad4cb51422e4b33753c74de367433f')


    def test_task_db(self):
        Task.objects.create(number=0, cbor='a')
        Task.objects.create(number=2, cbor='b')
        Task.objects.create(number=1, cbor='c')
        Task.objects.create(number=3, cbor='d')
        request = RequestFactory().post('/tasks/getAll/')
        view = TaskViewSet.getAll(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], [('a', 0), ('c', 1), ('b', 2), ('d', 3)])

class HelperApiTest(TestCase):
    """
    Testing the validate api endpoint.
    """
    def test_merkle_tree(self):
        test_data = [i for i in range(7)]
        output = merkleTree(test_data)
        self.assertEqual(output, 'a1a088cb5bf9e1926441bb39e9470dbd26dd5621ab5003e790e390866e7b7c51')

        test_data = [i for i in range(6)]
        output = merkleTree(test_data)
        self.assertEqual(output, 'eca1b2b0f605c7020003d09a348ad68ceec20a24b394561bf22058f71960c35d')

class ValidateApiTest(TestCase):
    """
    Testing the validate api endpoint.
    """
    def setUp(self):
        Account.objects.create(pkh="cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035")
        Account.objects.create(pkh="anotherpkhhere")
        Account.objects.create(pkh="feepkhhere")
        Token.objects.create(pid="", name="")

    def test_bad_data(self):
        test_data = dumps({}).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
    
    def test_missing_data(self):
        test_data = dumps([]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
    
    def test_bad_sub_data(self):
        test_data = dumps([[],[],[]]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
    
        test_data = dumps([{},{},[]]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
    
    def test_validate(self):
        test_data = dumps({
            "pkh":"cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035",
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        txBody = {
            "inputs":{
                "utxo1":10
            },
            "outputs":{
                "cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035": 4,
                "anotherpkhhere": 5
            },
            "fee":1,
        }
        txId = hashTxBody(txBody)
        txSign = {
            "pkh":"cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035",
            "data": txId,
            "sig": "845869a30127045820167b1ececd537d48298ee532952cff2dd13d2c565cb61c7ba9ae6317c06346dd6761646472657373583900cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e460356a87912a7ea1eee216e716b07d2718ed81b3f602240b14840b2b2bb1a166686173686564f45820934d19bdcc1eed3bc7388074a5ae951c89bb867fda1795d7221a4b0565748bcf58402bba79fb2feeb6dc3e2ff312539e953d952aa31fc557caa376b3bd0ea8f16f835e071786d326d3a28e58d54318fd7ce3fb89ff00d66b5478652151efb5f8c70d"
        }
        
        test_data = dumps([
            txBody,
            [txSign],
            "always_succeed"
        ]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
    
    def test_always_fails(self):
        test_data = dumps({
            "pkh":"cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035",
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        txBody = {
            "inputs":{
                "utxo1":10
            },
            "outputs":{
                "cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035": 4,
                "anotherpkhhere": 5
            },
            "fee":1,
        }
        txId = hashTxBody(txBody)
        txSign = {
            "pkh":"cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035",
            "data": txId,
            "sig": "845869a30127045820167b1ececd537d48298ee532952cff2dd13d2c565cb61c7ba9ae6317c06346dd6761646472657373583900cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e460356a87912a7ea1eee216e716b07d2718ed81b3f602240b14840b2b2bb1a166686173686564f45820934d19bdcc1eed3bc7388074a5ae951c89bb867fda1795d7221a4b0565748bcf58402bba79fb2feeb6dc3e2ff312539e953d952aa31fc557caa376b3bd0ea8f16f835e071786d326d3a28e58d54318fd7ce3fb89ff00d66b5478652151efb5f8c70d"
        }
        
        test_data = dumps([
            txBody,
            [txSign],
            "always_fails"
        ]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')

class NewAccountApiTest(TestCase):
    """
    Testing the newAccount api endpoint.
    
    /entries/newAccount/
    """

    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"
    test_pkh2 = "2ec0b67c151ef515d3ba0b46a08b160737e2cd40f8642112d33b9ecf51edb64dc602643d7964f0412ed4f61a5769c8734542618e9bf8f7e0"
    test_pkh3 = ""
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/newAccount/', {})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_payload_length(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh1})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(Account.objects.get(pkh=self.test_pkh1).pkh, self.test_pkh1)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')

        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh2})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Incorrect Length Key')

        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh3})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Incorrect Length Key')

    def test_new_account(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh1})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(Account.objects.get(pkh=self.test_pkh1).pkh, self.test_pkh1)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
    
    def test_double_account(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh1})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(Account.objects.get(pkh=self.test_pkh1).pkh, self.test_pkh1)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')

        request = RequestFactory().post('/entries/newAccount/', {'payload': self.test_pkh1})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.data['status'], 409)
        self.assertEqual(view.data['data'], 'Account Already Exists')

class NewUTxOApiTest(TestCase):
    """
    Testing the newUTxO api endpoint.
    
    /entries/newUTxO/
    """
    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"
    data1 = {
        "pkh":test_pkh1,
        "utxos": {
            "utxo1": {
                "": {
                    "": 10
                }
            }
        }
    }
    data2 = {
        "pkh":'test_pkh1',
        "utxos": {
            "utxo1": {
                "": {
                    "": 10
                }
            }
        }
    }
    data3 = {
        "pkh":test_pkh1,
        "utxos": {
            "utxo1": {
                "": {
                    "": 10
                }
            },
            "utxo1": {
                "": {
                    "": 10
                }
            }
        }
    }

    def setUp(self):
        Account.objects.create(pkh=self.test_pkh1)
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/newUTxO/', {})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_new_utxo(self):
        test_data = dumps(self.data1).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        # only creates a single entry
        self.assertEqual(len(Entry.objects.all()), 1)
        # find all entries from an account and check that the first utxo hash a value of 10.
        self.assertEqual(Entry.objects.get(account=Account.objects.get(pkh=self.test_pkh1)).utxo.value.all()[0].amount, 10)
    
    def test_no_account(self):
        test_data = dumps(self.data2).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)
    
    def test_double_add(self):
        test_data = dumps(self.data3).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        # only add one valid utxo
        self.assertEqual(len(Entry.objects.all()), 1)
    
    def test_bad_data(self):
        test_data = dumps({}).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
        self.assertEqual(len(Entry.objects.all()), 0)

        test_data = dumps({
            "pkh":"somekhhere",
            "utxos":[]
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
        self.assertEqual(len(Entry.objects.all()), 0)
    
        test_data = dumps({
            "pkh":"somekhhere",
            "utxos": {}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
        self.assertEqual(len(Entry.objects.all()), 0)

        # no token name or amt
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos": {"":{}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)

        # bad value
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos": {"":{"":0}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)

class DeleteUTxOsApiTest(TestCase):
    """
    Testing the deleteUTxOs api endpoint.
    """
    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"

    def setUp(self):
        Account.objects.create(pkh=self.test_pkh1)
        Token.objects.create(pid="", name="")

    def test_deleting_utxos(self):
        user = Account.objects.get(pkh=self.test_pkh1)
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":["utxo1"]
        }).hex()

        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 0)
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/deleteUTxOs/', {})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_bad_data_utxos(self):
        user = Account.objects.get(pkh=self.test_pkh1)
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])

        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({
            "pkh":"somephhere",
            "utxos":["utxo1"]
        }).hex()
        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 401)
        self.assertEqual(view.data['data'], 'No Account Exists')
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps([]).hex()
        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({}).hex()
        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
        self.assertEqual(len(Entry.objects.all()), 1)

    def test_no_utxos(self):
        # deleting something that doesn't exist gives a success but nothing is deleted
        user = Account.objects.get(pkh=self.test_pkh1)
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":["utxo2"]
        }).hex()
        
        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 1)

class GetUTxOsApiTest(TestCase):
    """
    Testing the getUTxOs api endpoint.

    /entries/getUTxOs/
    """
    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/getUTxOs/', {})
        view = EntryViewSet.getUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_getting_utxos(self):
        # Create some fake data
        Account.objects.create(pkh=self.test_pkh1).save()
        user = Account.objects.get(pkh=self.test_pkh1)
        
        Token.objects.create(pid="", name="").save() # ada
        token = Token.objects.get(pid="")
        
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()

        request = RequestFactory().post('/entries/getUTxOs/', {'payload': user.pkh})
        view = EntryViewSet.getUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], {'utxo1': {'': {'': 10}}}) # it returns the created utxo
    
    def test_no_entries(self):
        # Create some fake data
        Account.objects.create(pkh=self.test_pkh1).save()
        user = Account.objects.get(pkh=self.test_pkh1)
        
        request = RequestFactory().post('/entries/getUTxOs/', {'payload': user.pkh})
        view = EntryViewSet.getUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], {})
    
    def test_no_account(self):
        # Create some fake data
        request = RequestFactory().post('/entries/getUTxOs/', {'payload': 'user.pkh'})
        view = EntryViewSet.getUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 401)
        self.assertEqual(view.data['data'], 'No Account Exists')

class TotalAdaApiTest(TestCase):
    """
    Testing the total api endpoint.

    /entries/totalAda/
    """
    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/totalAda/', {})
        view = EntryViewSet.totalAda(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_correct_total(self):
        # Create some fake data
        Account.objects.create(pkh=self.test_pkh1).save()
        user = Account.objects.get(pkh=self.test_pkh1)
        
        Token.objects.create(pid="", name="").save() # ada
        token = Token.objects.get(pid="")
        
        Value.objects.create(token=token,amount=1000).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])

        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()

        request = RequestFactory().post('/entries/totalAda/', {'payload': user.pkh})
        view = EntryViewSet.totalAda(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 1000)
    
    def test_no_entries_total(self):
        # Create some fake data
        Account.objects.create(pkh=self.test_pkh1).save()
        user = Account.objects.get(pkh=self.test_pkh1)
        
        request = RequestFactory().post('/entries/totalAda/', {'payload': user.pkh})
        view = EntryViewSet.totalAda(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 0)
    
    def test_no_account_total(self):
        # Create some fake data
        request = RequestFactory().post('/entries/totalAda/', {'payload': 'user.pkh'})
        view = EntryViewSet.totalAda(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 401)
        self.assertEqual(view.data['data'], 'No Account Exists')

class HashingApiTest(TestCase):
    """"
    Testing for the hash api endpoint.
    """
    test_pkh1 = "54b22504fb5f504d5e1eaefa915940957ae530aa854bb8c6b403e80c"
    
    def test_no_payload(self):
        request = RequestFactory().post('/entries/hashTx/', {})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_good_data_object_hashing(self):
        test_data = dumps({
            "inputs":[],
            "outputs":{},
            "fee":0
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], '69fb181328ab941feccdb5093fd87d31d1bc2655bf76846ec42c293963ca5634')
    
    def test_bad_data_object_bad_values(self):
        test_data = dumps({
            "inputs":{},
            "outputs":[],
            "fee":'0'
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
    
    def test_bad_data_object_bad_keys(self):
        test_data = dumps({
            "1inputs":[],
            "1outputs":{},
            "1fee":0
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
    
    def test_bad_data_object_hashing(self):
        test_data = dumps({
            "inputs":{},
            "fee":0
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Fields')
    
    def test_incorrect_object_hashing(self):
        test_data = dumps([]).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')

class RandomNumberApiTest(TestCase):
    """
    Test a random number, crypto secure integer less than 2^64 -1
    """
    def test_no_payload(self):
        request = RequestFactory().get('/entries/randN/')
        view = EntryViewSet.randN(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertLessEqual(view.data['data'], pow(2, 64) - 1)