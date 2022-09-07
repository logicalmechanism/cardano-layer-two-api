curl -X POST http://localhost:8000/entries/newAccount/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=test'


curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=test'


curl -X POST http://localhost:8000/entries/newUTxO/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=a263706b686474657374657574786f73a1657574786f31a160a16001'