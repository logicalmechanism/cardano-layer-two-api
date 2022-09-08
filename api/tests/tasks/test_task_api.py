from django.test import RequestFactory, TestCase
from api.views import TaskViewSet
from api.models import Account, Task
from cbor2 import dumps

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

