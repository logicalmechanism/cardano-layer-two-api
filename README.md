```bash
curl -H 'Accept: application/json' \
-u admin:qwe123 \
-d 'pkh=somepkhhere' \
http://127.0.0.1:8000/entries/getUTxOs/
```

```bash
curl  -X POST -H 'Accept: application/json' \
-u admin:qwe123 \
-d 'pkh=somepkhhere' \
http://127.0.0.1:8000/entries/newAccount/
```