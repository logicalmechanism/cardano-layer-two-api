from django.test import RequestFactory, TestCase
from api.helper import merkleTree, constructTxBody, hashTxBody
from api.views import EntryViewSet
from cbor2 import dumps


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
    
    def test_bad_data_merkle_tree(self):
        test_data = 0
        output = merkleTree(test_data)
        self.assertEqual(output, '')

        test_data = []
        output = merkleTree(test_data)
        self.assertEqual(output, '')
    """
    Testing the tx body api.
    """
    def test_tx_body(self):
        test_data = {
            'inputs': {'a':1},
            'outputs': {'b':2},
            'fee': 0
        }
        output = constructTxBody(**test_data)
        self.assertEqual(output, 'a366696e70757473a1616101676f757470757473a16162026366656500')
    
    def test_bad_fee_tx_body(self):
        test_data = {
            'inputs': {},
            'outputs': {},
            'fee': -1
        }
        output = constructTxBody(**test_data)
        self.assertEqual(output, '')
    
    def test_bad_data_tx_body(self):
        test_data = {
            'inputs': [],
            'outputs': [],
            'fee': 0
        }
        output = constructTxBody(**test_data)
        self.assertEqual(output, '')

        test_data = {
            'inputs': {},
            'outputs': {},
            'fee': 0
        }
        output = constructTxBody(**test_data)
        self.assertEqual(output, '')
    """"
    Testing for the hash api endpoint.
    """
    def test_good_data_object_hashing(self):
        test_data = dumps({
            "inputs":{},
            "outputs":{},
            "fee":0
        }).hex()
        output = hashTxBody(test_data)
        self.assertEqual(output, '4a6ebcb565d4d7e74c3bbcf6aa515b26b104e60431bf4c4cc55efdccb1f0de58')
