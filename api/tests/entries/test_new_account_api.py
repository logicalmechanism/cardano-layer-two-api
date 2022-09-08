from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from api.models import Account

class NewAccountApiTest(TestCase):
    """
    Testing the newAccount api endpoint.
    """
    def test_new_account(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': "somepkhhere"})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(Account.objects.get(pkh='somepkhhere').pkh, "somepkhhere")
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
    
    def test_empty_account(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': ""})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_double_account(self):
        request = RequestFactory().post('/entries/newAccount/', {'payload': "somepkhhere"})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(Account.objects.get(pkh='somepkhhere').pkh, "somepkhhere")
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
        request = RequestFactory().post('/entries/newAccount/', {'payload': "somepkhhere"})
        view = EntryViewSet.newAccount(self, request)
        self.assertEqual(view.data['status'], 409)
        self.assertEqual(view.data['data'], 'Account Already Exists')
