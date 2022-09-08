from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from api.models import Entry, Account, UTxO, Value, Token

class TotalAdaApiTest(TestCase):
    """
    Testing the total api endpoint.
    """
    def test_correct_total(self):
        # Create some fake data
        Account.objects.create(pkh="somepkhhere").save()
        user = Account.objects.get(pkh="somepkhhere")
        
        Token.objects.create(pid="", name="").save() # ada
        token = Token.objects.get(pid="")
        
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])

        utxo = UTxO.objects.get(txId="utxo1")
        
        Entry.objects.create(account=user, utxo=utxo).save()

        request = RequestFactory().post('/entries/totalAda/', {'payload': user.pkh})
        view = EntryViewSet.totalAda(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 10)
    
    def test_no_entries_total(self):
        # Create some fake data
        Account.objects.create(pkh="somepkhhere").save()
        user = Account.objects.get(pkh="somepkhhere")
        
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

