# Cardano Layer 2

Users will lock ADA into a contract and will be issued an equal amount synthetic ADA on the layer 2. The synthetic ADA will be used to interact with dapps, games, and pay systems that are as fast as the users internet.

There are two different types of databases on layer 2. First and most importantly, there is the state db. This is the tip of the current global state of the layer. No on but top level auth tokens can update the state db. This list includes admins and the second db. The second and publicly available db is the task db. This database will contain every valid cbor transaction on layer 2. Access to this database will give any outside relay the power rebuild their own state db and audit the layer.

The system does not have a block but has a dynamically sized queue of valid transactions. The queue is always respect and is a FIFO. Transaction size does not matter as each valid transaction is just a list of crud actions. When the state db is succesfully updated from a valid transaction the cbor is stored in the task db.

To help federate the system we can create multiple types of auth tokens. There can be a relay, walllet, admin, etc. Initially, it should be very centralized then slowly build out the api to allow others to run complex relays.

A user will come to the site and create an account with their dapp enabled cardano wallet. This will trigger the create account api endpoint. Now the users payment public key hash is stored in the db and can be confirmed using the CIP08 verify function. The user will be prompted to enter an amount of REAL Ada to lock into the contract. The user will agree to a TOS then a transaction will be built to add their funds to the contract. The user will sign the add-to-contract transaction and they will sign the txId of the transaction, the hash#number. When the add-to-contract transaction has 5 confirmations the account on layer 2 will be updated with the new utxo. At this point, the user can interact with layer in a Tokhun wallet style of way. We will have to get wallets onboard to use this api to query the state db and use auth tokens to submit transactions but we can build out a great API that can handle all of that automatically.


### TODO

The task db and queue system.

Frontend.

Auth Tokens for relays / validators / wallets.

## Help
Install required node modules
```
cd api
npm install
```

Migrate the db. May need to do a force migrate.
```
python manage.py makemigrations
python manage.py migrate
```

Run api server
```
python manage.py runserver
```

Run unit tests
```
python manage.py test
```