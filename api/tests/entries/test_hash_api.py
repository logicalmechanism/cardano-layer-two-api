from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from cbor2 import dumps


class HashApiTest(TestCase):
    """"
    Testing for the hash api endpoint.
    """
    def test_good_data_object_hashing(self):
        test_data = dumps({
            "inputs":{},
            "outputs":{},
            "fee":0
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], '63bc09bd1a06d4c624ba0726ad0e2b00606e5902e65b42964edfdab04eaf5649')
    
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
