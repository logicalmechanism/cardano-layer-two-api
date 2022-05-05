from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from api.models import Entry, Account, UTxO, Value, Token
from cbor2 import dumps

class DeleteUTxOsApiTest(TestCase):
    """
    Testing the deleteUTxOs api endpoint.
    """
    def setUp(self):
        Account.objects.create(pkh="somepkhhere")
        Token.objects.create(pid="", name="")

    def test_deleting_utxos(self):
        user = Account.objects.get(pkh="somepkhhere")
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({
            "pkh":"somepkhhere",
            "utxos":["utxo1"]
        }).hex()

        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 0)
    
    def test_bad_data_utxos(self):
        user = Account.objects.get(pkh="somepkhhere")
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
    
    def test_no_utxos(self):
        user = Account.objects.get(pkh="somepkhhere")
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()
        self.assertEqual(len(Entry.objects.all()), 1)

        test_data = dumps({
            "pkh":"somepkhhere",
            "utxos":["utxo2"]
        }).hex()
        
        request = RequestFactory().post('/entries/deleteUTxOs/', {'payload': test_data})
        view = EntryViewSet.deleteUTxOs(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 1)
