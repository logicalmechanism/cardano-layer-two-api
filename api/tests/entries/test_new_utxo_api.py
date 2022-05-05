from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from api.models import Entry, Account
from cbor2 import dumps

class NewUTxOApiTest(TestCase):
    """
    Testing the newUTxO api endpoint.
    """
    def setUp(self):
        Account.objects.create(pkh="somepkhhere")
    
    def test_new_utxo(self):
        test_data = dumps({
            "pkh":"somepkhhere",
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 1)
        self.assertEqual(Entry.objects.get(account=Account.objects.get(pkh="somepkhhere")).utxo.value.all()[0].amount, 10)
    
    def test_no_account(self):
        test_data = dumps({
            "pkh":"somekhhere",
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)
    
    def test_double_add(self):
        test_data = dumps({
            "pkh":"somepkhhere",
            "utxos":{"utxo1":{"":{"":10}},"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        self.assertEqual(len(Entry.objects.all()), 1)
    
    def test_bad_data(self):
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

        test_data = dumps({
            "pkh":"somekhhere",
            "utxos": {"":{}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)

        test_data = dumps({
            "pkh":"somekhhere",
            "utxos": {"":{"":0}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
        self.assertEqual(len(Entry.objects.all()), 0)

