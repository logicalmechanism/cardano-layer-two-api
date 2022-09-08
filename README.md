# A Cardano Layer 2 Solution

Users will lock ADA into a contract and will be issued an equal amount synthetic ADA on the layer 2. The synthetic ADA will be used to interact with dapps, games, and pay systems that are as fast as the users internet.

There are two different types of databases on layer 2. First and most importantly, there is the state db. This is the tip of the current global state of the layer. No one but top level auth tokens can update the state db. This list includes admins and the second db. The second and publicly available db is the task db. This database will contain every valid cbor transaction on layer 2. Access to this database will give any outside relay the power rebuild their own state db and audit the layer.

The system does not have a block but has a dynamically sized queue of valid transactions. The queue is always respected and follows FIFO. Transaction size does not matter as each valid transaction is just a list of crud actions. When the state db is succesfully updated from a valid transaction the cbor is stored in the task db.

To help federate the system we can create multiple types of auth tokens. There can be a relay, walllet, admin, etc. Initially, it should be very centralized then slowly build out the api to allow others to run complex relays.

## Help
Install required node modules
```
cd api
npm install
```

Migrate the db. May need to do a force migrate with sync db.
```
python manage.py makemigrations
python manage.py migrate
```

Create a api token from a user, typically a superuser.
```
python manage.py drf_create_token <username>
```

Run api server
```
python manage.py runserver
```

Run unit tests
```
python manage.py test
```