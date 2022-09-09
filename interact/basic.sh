pkh='0199fc1a514a232b29a18db7635e6650b8cf65b65da83491532cb928dd5b7e1c5c4098962aca148e279363cc09e681268a5995d9d226e93b5d'

echo "curl -X POST http://localhost:8000/entries/newAccount/ \
-H 'Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8' \
--data 'payload=${pkh}'"

curl -X POST http://localhost:8000/entries/newAccount/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=${pkh}'




curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=test'


curl -X POST http://localhost:8000/entries/newUTxO/ \
-H "Authorization: Token f081d623f37f15270bc2a223049da07f1501abd8" \
--data 'payload=a263706b686474657374657574786f73a1657574786f31a160a16001'