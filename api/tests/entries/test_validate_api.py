from django.test import RequestFactory, TestCase
from api.views import EntryViewSet
from api.models import Account, Token
from api.helper import hashTxBody
from cbor2 import dumps


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
        # print(txId)
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
