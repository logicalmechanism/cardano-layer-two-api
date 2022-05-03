# Cardano Layer 2

### TODO

The task db and queue system.

The locking contract.

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