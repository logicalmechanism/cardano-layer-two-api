from django.test import RequestFactory, TestCase
from api.views import EntryViewSet, TaskViewSet
from api.models import Entry, Account, UTxO, Value, Token, Task, Datum, Redeemer
from api.helper import hashTxBody, merkleTree, constructTxBody, toCbor, fromCbor
from cbor2 import dumps, loads

class TaskApiTest(TestCase):
    """
    Testing the validate api endpoint.
    """
    goodCbor  = "83A366696E70757473A17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330A0676F757470757473A278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632A160A1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433A160A16005636665650181A463706B68787230306638313539356135653231356334636336336563383261303739306536366336623130393033336263333465323363303363643735366562353765336531346463656536626138663438623937303434636138363862346565303137643034656363373932646533383662656162373464646174617901B441333636363936453730373537343733413137383432333936363330333836323632333436343334363533343333333233333633363636313337363636333634333536363337333133393333333233393631363633313632333233383631363133313632363533323333333336363334363333303636363433343632363136313632333436323332363533393636333536343338363432333330413036373646373537343730373537343733413237383338363233353632333536313635333836353330333533303333333533353633333736333633333633343334333133353334333633383633333036333635333133333335363633343334333333353335363333333331333236333635363436363335333433373334333233333335363633324131363041313630303437383338333936323634333633363332363436313333333136343332333736313636363133343331333236343335333736343331333733303335363233373633333036353333363236353334363333353635363633323635333733333633333136323332333233383633333733363333363433334131363041313630303536333636363536353031637369677902E038343538343661323031323736373631363436343732363537333733353833393030663831353935613565323135633463633633656338326130373930653636633662313039303333626333346532336330336364373536656235376533653134646365653662613866343862393730343463613836386234656530313764303465636337393264653338366265616237346131363636383631373336383635363466343538646161333636363936653730373537343733613137383432333936363330333836323632333436343334363533343333333233333633363636313337363636333634333536363337333133393333333233393631363633313632333233383631363133313632363533323333333336363334363333303636363433343632363136313632333436323332363533393636333536343338363432333330613036373666373537343730373537343733613237383338363233353632333536313635333836353330333533303333333533353633333736333633333633343334333133353334333633383633333036333635333133333335363633343334333333353335363333333331333236333635363436363335333433373334333233333335363633326131363061313630303437383338333936323634333633363332363436313333333136343332333736313636363133343331333236343335333736343331333733303335363233373633333036353333363236353334363333353635363633323635333733333633333136323332333233383633333733363333363433336131363061313630303536333636363536353031353834303635666331313031313766623265383134353831666236616532393161613966303935616537373439663534646461376164373730613632623762646633663764313666373932656237353336356262616139366561393235636566346636343566623532313763616630333962316533623833643865393336396436323031636B657978546134303130313033323732303036323135383230363832363331663465386536303836306362336462333837306363346530363937653837383335333565306134353534303963663266633335613337353466646E616C776179735F73756363656564"
    badCbor   = "83a366696e70757473a1657574786f31a0676f757470757473a278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632a160a1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433a160a16005636665650181a463706b687872303066383135393561356532313563346363363365633832613037393065363663366231303930333362633334653233633033636437353665623537653365313464636565366261386634386239373034346361383638623465653031376430346563633739326465333836626561623734646461746179013861333636363936653730373537343733613136353735373437383666333161303637366637353734373037353734373361323738333836323335363233353631363533383635333033353330333333353335363333373633363333363334333433313335333433363338363333303633363533313333333536363334333433333335333536333333333133323633363536343636333533343337333433323333333536363332613136306131363030343738333833393632363433363336333236343631333333313634333233373631363636313334333133323634333533373634333133373330333536323337363333303635333336323635333436333335363536363332363533373333363333313632333233323338363333373336333336343333613136306131363030353633363636353635303163736967790264383435383436613230313237363736313634363437323635373337333538333930306638313539356135653231356334636336336563383261303739306536366336623130393033336263333465323363303363643735366562353765336531346463656536626138663438623937303434636138363862346565303137643034656363373932646533383662656162373461313636363836313733363836353634663435383963613336363639366537303735373437336131363537353734373836663331613036373666373537343730373537343733613237383338363233353632333536313635333836353330333533303333333533353633333736333633333633343334333133353334333633383633333036333635333133333335363633343334333333353335363333333331333236333635363436363335333433373334333233333335363633326131363061313630303437383338333936323634333633363332363436313333333136343332333736313636363133343331333236343335333736343331333733303335363233373633333036353333363236353334363333353635363633323635333733333633333136323332333233383633333733363333363433336131363061313630303536333636363536353031353834303864663638373462613131653533383831656135626537393034303230656562666465346566306363336466666232616566306361343165363439363461653162666265313635656566303830353966663839333438643730393365326562386335626638663766663761313335376236326466333030333061663431303036636b657978546134303130313033323732303036323135383230363832363331663465386536303836306362336462333837306363346530363937653837383335333565306134353534303963663266633335613337353466646e616c776179735f73756363656564"
    test_pkh1 = "00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74"
    test_pkh2 = "b5b5ae8e050355c7cc64415468c0ce135f44355c312cedf5474235f2"
    test_pkh3 = "9bd662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3"

    def setUp(self):
        Account.objects.create(pkh=self.test_pkh1)
        Account.objects.create(pkh=self.test_pkh2)
        Account.objects.create(pkh=self.test_pkh3)
        Token.objects.create(pid="", name="")

    def test_new_task_no_data(self):
        request = RequestFactory().post('/tasks/newTask/', {})
        view = TaskViewSet.newTask(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_new_task_bad_tx(self):
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        test_data = dumps({
            "number": 0,
            "cbor": self.badCbor
        }).hex()
        request = RequestFactory().post('/tasks/newTask/', {'payload': test_data})
        view = TaskViewSet.newTask(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')
    
    def test_new_task_good_data(self):
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)

        test_data = dumps({
            "number": 0,
            "cbor": self.goodCbor
        }).hex()
        request = RequestFactory().post('/tasks/newTask/', {'payload': test_data})
        view = TaskViewSet.newTask(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
    
    def test_new_task_good_data_bad_number(self):
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)

        test_data = dumps({
            "number": 1,
            "cbor": self.goodCbor
        }).hex()
        request = RequestFactory().post('/tasks/newTask/', {'payload': test_data})
        view = TaskViewSet.newTask(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_new_task_bad_data(self):
        test_data = dumps({
            "number": 0,
            "cbor": "testing"
        }).hex()
        request = RequestFactory().post('/tasks/newTask/', {'payload': test_data})
        view = TaskViewSet.newTask(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
    
    def test_task_db(self):
        Task.objects.create(number=0, cbor='a')
        Task.objects.create(number=2, cbor='b')
        Task.objects.create(number=1, cbor='c')
        Task.objects.create(number=3, cbor='d')
        request = RequestFactory().get('/tasks/getAll/')
        view = TaskViewSet.getAll(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], [('a', 0), ('c', 1), ('b', 2), ('d', 3)])
    
    def test_empty_task_db(self):
        request = RequestFactory().get('/tasks/getAll/')
        view = TaskViewSet.getAll(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], [])

class MerkleTreeApiTest(TestCase):
    """
    Testing the merkle tree api endpoint.
    """
    test_pkh1 = "00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74"
    def setUp(self):
        Account.objects.create(pkh=self.test_pkh1)
        Token.objects.create(pid="", name="")

    def test_no_payload(self):
        request = RequestFactory().post('/entries/getMerkleTree/', {})
        view = EntryViewSet.getMerkleTree(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_empty_payload(self):
        request = RequestFactory().post('/entries/getMerkleTree/', {'payload': ''})
        view = EntryViewSet.getMerkleTree(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')
    
    def test_good_payload(self):
        user = Account.objects.get(pkh=self.test_pkh1)
        token = Token.objects.get(pid="")
        Value.objects.create(token=token,amount=10).save()
        value = Value.objects.get(token=token)
        
        u = UTxO.objects.create(txId="utxo1")
        u.value.set([value])
        utxo = UTxO.objects.get(txId="utxo1")
        Entry.objects.create(account=user, utxo=utxo).save()

        request = RequestFactory().post('/entries/getMerkleTree/', {'payload': self.test_pkh1})
        view = EntryViewSet.getMerkleTree(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], '9f08bb4d4e4323cfa7fcd5f719329af1b28aa1be233f4c0fd4baab4b2e9f5d8d')
    
    def test_merkle_tree(self):
        test_data = [i for i in range(0)]
        output = merkleTree(test_data)
        self.assertEqual(output, '')

        test_data = [i for i in range(1)]
        output = merkleTree(test_data)
        self.assertEqual(output, '0fd923ca5e7218c4ba3c3801c26a617ecdbfdaebb9c76ce2eca166e7855efbb8')

        test_data = [i for i in range(2)]
        output = merkleTree(test_data)
        self.assertEqual(output, '45f3d3ed97ca49b86f1ae514e55e69e0fba32124aa23eb4b70260f89f259271b')

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
    test_pkh1 = "00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74"
    test_pkh2 = "b5b5ae8e050355c7cc64415468c0ce135f44355c312cedf5474235f2"
    test_pkh3 = "9bd662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3"
    test_pkh4 = "000662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3"

    def setUp(self):
        Account.objects.create(pkh=self.test_pkh1)
        Account.objects.create(pkh=self.test_pkh2)
        Account.objects.create(pkh=self.test_pkh3)

        Token.objects.create(pid="", name="")

    def test_no_data(self):
        request = RequestFactory().post('/entries/validate/', {})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Missing Data')

    def test_bad_data(self):
        test_data = dumps({}).hex() # should be a list
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
        test_data = dumps([{},{},[]]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
    
        test_data = dumps([[],{},'']).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Wrong Data Type')
    
    def test_validate_with_bad_accounts(self):
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        txBody = {
            "inputs":{
                "9f08bb4d4e4323cfa7fcd5f719329af1b28aa1be233f4c0fd4baab4b2e9f5d8d#0":{}
            },
            "outputs":{
                self.test_pkh4: {"":{"":4}},
                self.test_pkh3: {"":{"":5}}
            },
            "fee":1,
        }
        txId = constructTxBody(**txBody)
        txSign = {
            "pkh": self.test_pkh1,
            "data": txId,
            "sig": "845846a201276761646472657373583900f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74a166686173686564f458daa366696e70757473a17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330a0676f757470757473a278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632a160a1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433a160a160056366656501584065fc110117fb2e814581fb6ae291aa9f095ae7749f54dda7ad770a62b7bdf3f7d16f792eb75365bbaa96ea925cef4f645fb5217caf039b1e3b83d8e9369d6201",
            "key": "a4010103272006215820682631f4e8e60860cb3db3870cc4e0697e8783535e0a455409cf2fc35a3754fd"
        }
        
        test_data = dumps([
            txBody,
            [txSign],
            "always_succeed"
        ]).hex()
        request = RequestFactory().post('/entries/validate/', {'payload': test_data})
        view = EntryViewSet.validate(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 400)
        self.assertEqual(view.data['data'], 'Fail')

    def test_validate(self):
        test_data = dumps({
            "pkh":self.test_pkh1,
            "utxos":{"utxo1":{"":{"":10}}}
        }).hex()
        request = RequestFactory().post('/entries/newUTxO/', {'payload': test_data})
        view = EntryViewSet.newUTxO(self, request)
        txBody = {
            "inputs":{
                "9f08bb4d4e4323cfa7fcd5f719329af1b28aa1be233f4c0fd4baab4b2e9f5d8d#0":{}
            },
            "outputs":{
                self.test_pkh2: {"":{"":4}},
                self.test_pkh3: {"":{"":5}}
            },
            "fee":1,
        }
        txId = constructTxBody(**txBody)
        txSign = {
            "pkh": self.test_pkh1,
            "data": txId,
            "sig": "845846a201276761646472657373583900f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74a166686173686564f458daa366696e70757473a17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330a0676f757470757473a278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632a160a1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433a160a160056366656501584065fc110117fb2e814581fb6ae291aa9f095ae7749f54dda7ad770a62b7bdf3f7d16f792eb75365bbaa96ea925cef4f645fb5217caf039b1e3b83d8e9369d6201",
            "key": "a4010103272006215820682631f4e8e60860cb3db3870cc4e0697e8783535e0a455409cf2fc35a3754fd"
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
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')

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
        "pkh": test_pkh1,
        "utxos": {
            "utxo1": {"": {"": 10}}
        }
    }
    data2 = {
        "pkh": 'test_pkh1',
        "utxos": {
            "utxo1": {"": {"": 10}}
        }
    }
    data3 = {
        "pkh": test_pkh1,
        "utxos": {
            "utxo1": {"": {"": 10}},
            "utxo1": {"": {"": 5}}
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
        self.assertEqual(view.data['data'], 'Wrong Data Type')
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
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], 'Success')
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
            "inputs":{},
            "outputs":{},
            "fee":0
        }).hex()
        request = RequestFactory().post('/entries/hashTx/', {'payload': test_data})
        view = EntryViewSet.hashTx(self, request)
        self.assertEqual(view.status_code, 200)
        self.assertEqual(view.data['status'], 200)
        self.assertEqual(view.data['data'], '63bc09bd1a06d4c624ba0726ad0e2b00606e5902e65b42964edfdab04eaf5649')
    
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

class CborApiTest(TestCase):
    """
    Test the to and from Cbor functions.
    """
    def test_to_cbor_empty(self):
        test_data = {}
        output = toCbor(test_data)
        self.assertEqual(output, 'a0')
    
    def test_to_cbor_nested(self):
        test_data = [{},{},'']
        output = toCbor(test_data)
        self.assertEqual(output, '83a0a060')
    
    def test_to_cbor_good_data(self):
        test_data = [{},{},0]
        output = toCbor(test_data)
        self.assertEqual(output, '83a0a000')
    
    def test_to_cbor_string(self):
        test_data = 'test'
        output = toCbor(test_data)
        self.assertEqual(output, '6474657374')
    
    def to_from_cbor_string(self):
        test_data = "6474657374"
        output = fromCbor(test_data)
        self.assertEqual(output, 'test')
    
    def to_from_cbor_nested(self):
        test_data = "83a0a000"
        output = fromCbor(test_data)
        self.assertEqual(output, [{},{},0])